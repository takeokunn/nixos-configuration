{ pkgs }:
with pkgs; [
  # for emacs
  emacs-git
  pinentry-emacs
  emacsPackages.mu4e
]
