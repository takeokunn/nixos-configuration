{
  epkgs,
  pkgs,
  nurPkgs,
}:
let
  explain-pause-mode = nurPkgs.emacs-explain-pause-mode;
in
with epkgs;
[
  proced-narrow
  symon
  esup
  explain-pause-mode
  disk-usage
  keyfreq
  uptimes
]
