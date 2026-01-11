{
  pkgs,
  lib ? pkgs.lib,
}:
let
  inherit (pkgs) buildNpmPackage fetchzip stdenv;

  # Helper for npm CLI packages from registry
  mkNpmPackage =
    {
      pname,
      npmName ? pname,
      version,
      hash,
      npmDepsHash,
      description,
      homepage,
      license ? lib.licenses.mit,
      mainProgram ? pname,
      forceEmptyCache ? false,
      npmFlags ? [ ],
      env ? { },
      postInstall ? "",
    }:
    buildNpmPackage rec {
      inherit
        pname
        version
        npmDepsHash
        npmFlags
        env
        postInstall
        ;
      inherit forceEmptyCache;

      src = fetchzip {
        url = "https://registry.npmjs.org/${npmName}/-/${pname}-${version}.tgz";
        inherit hash;
      };

      postPatch = ''
        cp ${./${pname}/package-lock.json} package-lock.json
        mkdir -p node_modules
      '';

      dontNpmBuild = true;

      meta = {
        inherit
          description
          homepage
          license
          mainProgram
          ;
      };
    };

  # Helper for platform-specific binary packages (copilot)
  mkCopilotPlatformPackage =
    {
      platform,
      version,
      hash,
      nixPlatform,
    }:
    stdenv.mkDerivation rec {
      pname = "copilot-language-server-${platform}";
      inherit version;

      src = fetchzip {
        url = "https://registry.npmjs.org/@github/copilot-language-server-${platform}/-/copilot-language-server-${platform}-${version}.tgz";
        inherit hash;
      };

      dontBuild = true;

      installPhase = ''
        runHook preInstall
        mkdir -p $out/lib/node_modules/@github/copilot-language-server-${platform}
        cp -r . $out/lib/node_modules/@github/copilot-language-server-${platform}/
        runHook postInstall
      '';

      meta = {
        description = "GitHub Copilot Language Server (${platform})";
        homepage = "https://github.com/github/copilot";
        license = lib.licenses.mit;
        platforms = [ nixPlatform ];
      };
    };

  # Copilot version (shared across all platform packages)
  copilotVersion = "1.408.0";
in
{
  ccusage = mkNpmPackage {
    pname = "ccusage";
    version = "18.0.5";
    hash = "sha256-i4UyRU7EA0PLduABnPGbcD8I06ZjmjwXCC77vtFM638=";
    npmDepsHash = "sha256-HiSZ1KI86bKOmv8vctekvXy9d2+wCZiVCfEE7Cehuzc=";
    forceEmptyCache = true;
    description = "Claude AI cost tracking CLI for monitoring API usage";
    homepage = "https://github.com/ryoppippi/ccusage";
  };

  "@anthropic-ai/claude-code" = mkNpmPackage {
    pname = "claude-code";
    npmName = "@anthropic-ai/claude-code";
    version = "2.1.4";
    hash = "sha256-+trjrFphFJ4fzTu0Ij7LlO8nR90Ri3MKhPyJAin9OS8=";
    npmDepsHash = "sha256-v9YSPu4xq5F70TkPy8GyhS8VebPIyfay1luuWG+AXn0=";
    npmFlags = [ "--omit=optional" ];
    forceEmptyCache = true;
    env.AUTHORIZED = "1";
    postInstall = ''
      wrapProgram $out/bin/claude \
        --set DISABLE_AUTOUPDATER 1 \
        --unset DEV
    '';
    description = "Agentic coding tool that lives in your terminal";
    homepage = "https://github.com/anthropics/claude-code";
    license = lib.licenses.unfree;
    mainProgram = "claude";
  };

  "@openai/codex" = mkNpmPackage {
    pname = "codex";
    npmName = "@openai/codex";
    version = "0.80.0";
    hash = "sha256-fUCJChIJzocFT65XNrsRkK4V45YXckel7RD6zZh47SY=";
    npmDepsHash = "sha256-SpFBhYRwcX4jMcTBW+Gtjx9b6QRjpQFlp9xlGvT16dQ=";
    forceEmptyCache = true;
    description = "Codex CLI from OpenAI - an AI coding assistant";
    homepage = "https://github.com/openai/codex";
    license = lib.licenses.asl20;
  };

  "@github/copilot-language-server" = mkNpmPackage {
    pname = "copilot-language-server";
    npmName = "@github/copilot-language-server";
    version = copilotVersion;
    hash = "sha256-7+A2rGqf8/TNRrDaPaHWWGayGk0O4IGlF6LoeQi1Mfg=";
    npmDepsHash = "sha256-m6wj8PpHNbl3p4XH0WevPcHcPlmVJZFBcAjfn37+It0=";
    npmFlags = [ "--omit=optional" ];
    description = "GitHub Copilot Language Server";
    homepage = "https://github.com/github/copilot";
  };

  "@github/copilot-language-server-darwin-arm64" = mkCopilotPlatformPackage {
    platform = "darwin-arm64";
    version = copilotVersion;
    hash = "sha256-IGtWbvUkGa3X22cqaz7jFzBSB/Z0TCvjFvRkt+vxFIY=";
    nixPlatform = "aarch64-darwin";
  };

  "@github/copilot-language-server-darwin-x64" = mkCopilotPlatformPackage {
    platform = "darwin-x64";
    version = copilotVersion;
    hash = "sha256-nuAimuEh+Mfe4iqvJiyuGTlcDCHUe9rZBqY+uogsglI=";
    nixPlatform = "x86_64-darwin";
  };

  "@github/copilot-language-server-linux-x64" = mkCopilotPlatformPackage {
    platform = "linux-x64";
    version = copilotVersion;
    hash = "sha256-oZwP2xfv/yyfmvi26M6VsD9slK0uC/OYAmB3199mesY=";
    nixPlatform = "x86_64-linux";
  };

  "@github/copilot-language-server-linux-arm64" = mkCopilotPlatformPackage {
    platform = "linux-arm64";
    version = copilotVersion;
    hash = "sha256-i/J+0SDSkFsmBTgwaEHF6mU29XqSarpk7Q4R2K86cIE=";
    nixPlatform = "aarch64-linux";
  };
}
