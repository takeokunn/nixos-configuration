{ lib, stdenv, stdenvNoCC, fetchzip, installShellFiles }:

let
  pname = "mitamae";
  version = "1.14.1";

  _meta = with lib; {
    description =
      "mitamae is a fast, simple, and single-binary configuration management tool with a DSL like Chef";
    homepage = "https://github.com/itamae-kitchen/mitamae";
    changelog =
      "https://github.com/itamae-kitchen/mitamae/blob/v${version}/CHANGELOG.md";
    license = licenses.mit;
    platforms = platforms.darwin;
    mainProgram = "mitamae";
  };
in {
  aarch64-darwin = stdenvNoCC.mkDerivation {
    inherit pname version;

    src = fetchzip {
      url =
        "https://github.com/itamae-kitchen/mitamae/releases/download/v${version}/mitamae-aarch64-darwin.tar.gz";
      hash = "sha256-NjPGySXq1aAVatzLI4t6M5+1ZECXZio1AFTvg/s1R4Y=";
    };

    nativeBuildInputs = [ installShellFiles ];

    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p $out/bin
      cp -r mitamae-aarch64-darwin $out/bin/mitamae

      runHook postInstall
    '';

    meta = _meta // {
      sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    };
  };
}.${stdenv.hostPlatform.system} or (throw
  "Unsupported platform ${stdenv.hostPlatform.system}")
