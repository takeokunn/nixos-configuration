{
  epkgs,
  pkgs,
  nurPkgs,
}:
let
  mu4e-dashboard = nurPkgs.emacs-mu4e-dashboard;
in
with epkgs;
[
  # Client
  md4rd

  # Googling
  google-translate

  # Mail
  mu4e
  mu4e-views
  mu4e-dashboard
]
