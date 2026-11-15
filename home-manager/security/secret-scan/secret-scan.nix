# Builds secret-scan.sh with store paths for every dependency, since launchd starts it with a
# minimal PATH. `gitleaks` is a parameter so the flake check can substitute a failing one.
{
  pkgs,
  gitleaks ? pkgs.gitleaks,
}:
let
  # gitleaks built-in rules only. The home-manager gitleaks config is user-extensible, and an
  # allowlist added there for one repository would also hide findings in the logs.
  gitleaksConfig = (pkgs.formats.toml { }).generate "secret-scan-gitleaks.toml" {
    extend.useDefault = true;
  };
in
pkgs.writeScript "secret-scan" (
  builtins.replaceStrings
    [ "#!/usr/bin/env bash" "@PATH@" "@GITLEAKS@" "@GITLEAKS_CONFIG@" ]
    [
      "#!${pkgs.bash}/bin/bash"
      (pkgs.lib.makeBinPath [
        pkgs.coreutils
        pkgs.findutils
        pkgs.jq
      ])
      "${gitleaks}/bin/gitleaks"
      "${gitleaksConfig}"
    ]
    (builtins.readFile ./secret-scan.sh)
)
