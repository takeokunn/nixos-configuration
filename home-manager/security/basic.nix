{ pkgs }:
let
  gnupg = import ./gnupg;
  password-store = import ./password-store { inherit pkgs; };
in
[ ./modules/gitleaks gnupg password-store ]
