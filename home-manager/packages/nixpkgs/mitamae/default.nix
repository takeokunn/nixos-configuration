{ lib, stdenvNoCC, fetchzip, installShellFiles, system, ... }:
let
  pname = "mitamae";
  version = "1.14.1";
in stdenvNoCC.mkDerivation {
  inherit pname version;

  src = fetchzip {
    url =
      "https://github.com/itamae-kitchen/mitamae/releases/download/v${version}/mitamae-${system}.tar.gz";
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

  meta = with lib; {
    description =
      "mitamae is a fast, simple, and single-binary configuration management tool with a DSL like Chef";
    homepage = "https://github.com/itamae-kitchen/mitamae";
    changelog =
      "https://github.com/itamae-kitchen/mitamae/blob/v${version}/CHANGELOG.md";
    license = licenses.mit;
    mainProgram = "mitamae";
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
}
