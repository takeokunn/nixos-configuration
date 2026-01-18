{
  epkgs,
  pkgs,
  nurPkgs,
}:
let
  eshell-multiple = nurPkgs.emacs-eshell-multiple;
in
with epkgs;
[
  esh-help
  eshell-did-you-mean
  eshell-z
  fish-completion
  eshell-multiple
  eshell-syntax-highlighting
  eshell-fringe-status
]
