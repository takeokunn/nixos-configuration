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
  copilotVersion = "1.409.0";
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
    version = "2.1.7";
    hash = "sha256-M2ZLGnrvNki7B2jOh4Uq2SfSxkICh76uRIFogq+kKZ8=";
    npmDepsHash = "sha256-Y+4ZcfEJJg4/XYc3vNLw4R5OJz3FlYvgQpkB739jKAQ=";
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
    hash = "sha256-C8ThNi2YONluLh4Zgmvw+trUAc7dHv9fl+tHiyovtYc=";
    npmDepsHash = "sha256-UYWBNwvE9d76K4YNbEc/kqTeYZGeKzgysDYRWpuiPTM=";
    npmFlags = [ "--omit=optional" ];
    description = "GitHub Copilot Language Server";
    homepage = "https://github.com/github/copilot";
  };

  "@github/copilot-language-server-darwin-arm64" = mkCopilotPlatformPackage {
    platform = "darwin-arm64";
    version = copilotVersion;
    hash = "sha256-3r8NEyyuNm7HhWlTz4aabOMMPgCuWUs29oDtGTRLHs8=";
    nixPlatform = "aarch64-darwin";
  };

  "@github/copilot-language-server-darwin-x64" = mkCopilotPlatformPackage {
    platform = "darwin-x64";
    version = copilotVersion;
    hash = "sha256-YUZQBike0gjrLDkIgWKLt7dgF2FFAYYBLLwBkGPIHZA=";
    nixPlatform = "x86_64-darwin";
  };

  "@github/copilot-language-server-linux-x64" = mkCopilotPlatformPackage {
    platform = "linux-x64";
    version = copilotVersion;
    hash = "sha256-UHMzhQJHWEaCf1fV6L+yIw+Su2t0Aq48leZX9sfJDoE=";
    nixPlatform = "x86_64-linux";
  };

  "@github/copilot-language-server-linux-arm64" = mkCopilotPlatformPackage {
    platform = "linux-arm64";
    version = copilotVersion;
    hash = "sha256-GtlXVsSiRqVGTSNRD+HitqxsC+odgJyV1wCLuS98cmQ=";
    nixPlatform = "aarch64-linux";
  };
}
