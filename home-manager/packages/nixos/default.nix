{ pkgs }:
with pkgs; [
  pinentry-curses

  # for emacs
  emacs-git
  mu
  emacsPackages.mu4e
]
