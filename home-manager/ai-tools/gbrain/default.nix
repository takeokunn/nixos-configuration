{
  config,
  lib,
  pkgs,
  nurPkgs,
  ollama-bge-m3,
  ...
}:
let
  gbrain = nurPkgs.gbrain;
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;

  # gbrain resolves its brain from HOME (~/.gbrain); every command below relies on that default.
  gbrainHome = "${config.home.homeDirectory}/.gbrain";
  tokenFile = "${gbrainHome}/agent-token";
  tokenName = "agents";
  port = 3131;
  url = "http://127.0.0.1:${toString port}/mcp";

  # Embeddings come from the local Ollama below; gbrain's ollama recipe reads this base URL.
  ollamaUrl = "http://${config.services.ollama.host}:${toString config.services.ollama.port}";
  bgeM3 = import ./ollama-model.nix { inherit lib pkgs; } {
    name = "bge-m3";
    manifest = ollama-bge-m3;
  };
  # The name carries the manifest digest, so a lockfile update that moves the tag changes the model
  # gbrain records, and the serve script re-embeds the brain instead of mixing two models' vectors.
  embeddingModel = "ollama:${bgeM3.ollamaName}";
  # bge-m3 emits 1024-dimension vectors, the width a keyless gbrain brain is created with.
  embeddingDimensions = "1024";

  # Claude Code and Codex start a stdio MCP server per session, and a PGLite brain admits one owner:
  # every session after the first would get only gbrain_status. One resident HTTP server per host
  # holds the brain instead. `auth create` and the setup commands open the datastore themselves, so
  # they run here, before serve takes the lock.
  serve = pkgs.writeShellApplication {
    name = "gbrain-serve";
    runtimeInputs = [
      gbrain
      pkgs.coreutils
      pkgs.curl
      pkgs.gnugrep
      pkgs.jq
    ];
    text = ''
      umask 077
      mkdir -p ${lib.escapeShellArg gbrainHome}
      # mkdir -p leaves an existing directory's mode alone, such as one impermanence recreated.
      chmod 700 ${lib.escapeShellArg gbrainHome}
      # A fixed directory: gbrain loads bunfig.toml and .env from the directory it starts in.
      cd ${lib.escapeShellArg gbrainHome}

      if [ ! -e config.json ]; then
        # A token file left from an earlier brain names no token in the new one.
        rm -f ${lib.escapeShellArg tokenFile}
        gbrain init --pglite --no-embedding
      fi
      gbrain apply-migrations --yes --no-autopilot-install
      gbrain config set self_upgrade.mode off
      # Skills come from Nix; the brain's own skillpack would be offered to every connecting agent.
      gbrain config set mcp.publish_skills false
      gbrain sources shared-skills default off
      # Extract durable claims (preferences, corrections, decisions) from the turns and transcripts
      # the agents' capture hooks bank (shared/*-managed-settings.nix).
      gbrain config set memory.auto_writeback salient
      # The reranker gbrain's default search mode calls is Voyage's, which has no key here.
      gbrain config set search.reranker.enabled false

      # Moving the brain onto the declared model needs the single-writer lock, so it runs here,
      # until the database records that model. A failure exits before serve and launchd/systemd
      # retry, instead of serving keyword-only or with vectors from two models; both commands resume
      # an interrupted run. A keyless brain takes `enable` first, which sets only the runtime config
      # plane. The migration records the model in the database and embeds every page and fact
      # again; Ollama bills nothing, so a $0 cap admits all of it.
      embedding_status=$(gbrain migrate embeddings --status --json)
      if [ "$(jq -r .db_plane.model <<<"$embedding_status")" != ${embeddingModel} ]; then
        curl --fail --silent --show-error --output /dev/null \
          --retry 30 --retry-delay 2 --retry-connrefused \
          --data ${
            lib.escapeShellArg (builtins.toJSON { model = bgeM3.ollamaName; })
          } ${ollamaUrl}/api/show
        if [ "$(jq -r .file_plane.model <<<"$embedding_status")" = null ]; then
          gbrain embeddings enable --embedding-model ${embeddingModel} --embedding-dimensions ${embeddingDimensions}
        fi
        gbrain migrate embeddings --to ${embeddingModel} --dim ${embeddingDimensions} --yes --max-cost-usd 0
      fi

      if [ ! -s ${lib.escapeShellArg tokenFile} ]; then
        # The token prints once and lives only in the database, so a lost file leaves an orphan
        # token behind; revoke it before minting the replacement.
        if ! revoke_output=$(gbrain auth revoke ${tokenName} 2>&1); then
          case "$revoke_output" in
            *"No active token found"*) ;;
            *)
              printf '%s\n' "$revoke_output" >&2
              exit 1
              ;;
          esac
        fi
        create_output=$(gbrain auth create ${tokenName} --scopes read,write)
        if ! token=$(printf '%s\n' "$create_output" | grep -E '^ +gbrain_[A-Za-z0-9_-]+$' | tr -d ' '); then
          echo "gbrain-serve: no token in the gbrain auth create output" >&2
          exit 1
        fi
        if [ "$(printf '%s\n' "$token" | grep -c .)" -ne 1 ]; then
          echo "gbrain-serve: could not read exactly one token from gbrain auth create" >&2
          exit 1
        fi
        tmp=$(mktemp ${lib.escapeShellArg "${tokenFile}.XXXXXX"})
        printf '%s\n' "$token" >"$tmp"
        mv "$tmp" ${lib.escapeShellArg tokenFile}
      fi

      # --surface caps what any token may call, and the token's read,write scope still refuses the
      # admin-scope operations. --advertise keeps tools/list to the seven memory verbs; request_tools
      # describes the rest.
      exec gbrain serve --http --bind 127.0.0.1 --port ${toString port} --surface full --advertise verbs
    '';
  };

  # Claude Code's headersHelper and Codex's http_headers_helper both read one JSON object of string
  # headers from stdout, so the token never lands in a config file or the environment.
  headersHelper = pkgs.writeShellScript "gbrain-mcp-headers" ''
    token=$(${pkgs.coreutils}/bin/head -n 1 ${lib.escapeShellArg tokenFile}) || exit 1
    case "$token" in
      gbrain_*) ;;
      *)
        echo "gbrain-mcp-headers: no token in ${tokenFile}" >&2
        exit 1
        ;;
    esac
    case "$token" in
      *[!A-Za-z0-9_-]*)
        echo "gbrain-mcp-headers: malformed token in ${tokenFile}" >&2
        exit 1
        ;;
    esac
    printf '{"Authorization":"Bearer %s"}\n' "$token"
  '';

  # The managed settings of both agents (shared/*-managed-settings.nix) run this path, which a system
  # module cannot derive from home-manager, so the assertion below pins it.
  hookRelPath = ".local/share/gbrain/hook";
  hook = pkgs.writeShellScript "gbrain-hook" ''
    # The session directory reaches gbrain through the hook's stdin `cwd`; starting there would load
    # its bunfig.toml and .env.
    cd ${lib.escapeShellArg gbrainHome} || {
      echo "gbrain-hook: ${gbrainHome} is missing; is the gbrain service running?" >&2
      exit 1
    }
    # These select the brain or the hook's behaviour; a project shell that exports them would point
    # the hook away from the resident server.
    exec ${pkgs.coreutils}/bin/env -u DATABASE_URL -u GBRAIN_DATABASE_URL -u GBRAIN_HOME \
      -u GBRAIN_SOURCE -u GBRAIN_HOOKS -u GBRAIN_HOOK_LANE ${gbrain}/bin/gbrain hook "$@"
  '';
in
{
  assertions = [
    {
      assertion = config.xdg.dataHome == "${config.home.homeDirectory}/.local/share";
      message = "gbrain: the managed hook settings expect the hook at ~/${hookRelPath}, but xdg.dataHome is ${config.xdg.dataHome}";
    }
  ];

  home.packages = [ gbrain ];
  home.file = {
    ${hookRelPath}.source = hook;
  }
  // bgeM3.files;

  services.ollama.enable = true;

  # Ollama holds only the models declared here. home-manager swaps its own links on each switch;
  # this removes whatever else is under ~/.ollama/models (a hand pull, a partial download), so no
  # model reaches Ollama outside Nix.
  home.activation.ollamaModels =
    let
      models = "${config.home.homeDirectory}/.ollama/models";
      declared = map (lib.removePrefix ".ollama/models/") (lib.attrNames bgeM3.files);
    in
    lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      for dir in manifests blobs; do
        [ -d ${lib.escapeShellArg models}/"$dir" ] || continue
        while IFS= read -r -d "" file; do
          case "''${file#${lib.escapeShellArg models}/}" in
            ${lib.concatMapStringsSep " | " lib.escapeShellArg declared}) ;;
            *) run rm -f "$file" ;;
          esac
        done < <(${pkgs.findutils}/bin/find ${lib.escapeShellArg models}/"$dir" -mindepth 1 ! -type d -print0)
        run ${pkgs.findutils}/bin/find ${lib.escapeShellArg models}/"$dir" -mindepth 1 -type d -empty -delete
      done
    '';

  launchd.agents.gbrain = lib.mkIf isDarwin {
    enable = true;
    config = {
      ProgramArguments = [ "${serve}/bin/gbrain-serve" ];
      RunAtLoad = true;
      KeepAlive = true;
      ThrottleInterval = 10;
      # launchd opens these before the script creates ~/.gbrain, so they live in a directory that exists.
      StandardOutPath = "${config.home.homeDirectory}/Library/Logs/gbrain.log";
      StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/gbrain.log";
    };
  };

  systemd.user.services.gbrain = lib.mkIf (!isDarwin) {
    Unit = {
      Description = "gbrain MCP server for coding agents";
      # launchd has no ordering, so gbrain-serve also waits for Ollama itself.
      Wants = [ "ollama.service" ];
      After = [ "ollama.service" ];
    };
    Service = {
      ExecStart = "${serve}/bin/gbrain-serve";
      Restart = "on-failure";
      RestartSec = 10;
    };
    Install.WantedBy = [ "default.target" ];
  };

  programs.claude-code.mcpServers.gbrain = {
    type = "http";
    inherit url;
    headersHelper = "${headersHelper}";
  };

  programs.codex.settings.mcp_servers.gbrain = {
    inherit url;
    http_headers_helper = "${headersHelper}";
  };
}
