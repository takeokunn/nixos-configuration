{
  epkgs,
  nurPkgs,
  pkgs,
}:
let
  mu4e-dashboard = nurPkgs.emacs-mu4e-dashboard;
in
with epkgs;
[
  md4rd

  google-translate
]
++ pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [
  mu4e
  mu4e-views
  mu4e-dashboard
]
