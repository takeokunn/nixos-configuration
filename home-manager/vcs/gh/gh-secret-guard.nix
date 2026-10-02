# Builds gh-secret-guard.sh as a script standing in for `gh`. `gh` is a parameter so the flake check
# can substitute a stub that records what it was called with.
{ pkgs, gh }:
let
  # gitleaks' built-in rules only. The home-manager gitleaks config is not reused: it is
  # user-extensible, and an allowlist added there for one repository's false positive would also
  # open this guard.
  gitleaksConfig = (pkgs.formats.toml { }).generate "gh-secret-guard-gitleaks.toml" {
    extend.useDefault = true;
  };
in
pkgs.writeShellScript "gh" (
  builtins.replaceStrings
    [ "@GH@" "@GITLEAKS@" "@GITLEAKS_CONFIG@" "@JQ@" "@GIT@" ]
    [
      "${gh}"
      "${pkgs.gitleaks}/bin/gitleaks"
      "${gitleaksConfig}"
      "${pkgs.jq}/bin/jq"
      "${pkgs.git}/bin/git"
    ]
    (builtins.readFile ./gh-secret-guard.sh)
)
