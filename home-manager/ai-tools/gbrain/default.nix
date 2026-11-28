{
  config,
  lib,
  pkgs,
  nurPkgs,
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

  # Claude Code and Codex start a stdio MCP server per session, and a PGLite brain admits one owner:
  # every session after the first would get only gbrain_status. One resident HTTP server per host
  # holds the brain instead. `auth create` and the setup commands open the datastore themselves, so
  # they run here, before serve takes the lock.
  serve = pkgs.writeShellApplication {
    name = "gbrain-serve";
    runtimeInputs = [
      gbrain
      pkgs.coreutils
      pkgs.gnugrep
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
  home.file.${hookRelPath}.source = hook;

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
    Unit.Description = "gbrain MCP server for coding agents";
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
