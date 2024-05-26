{ pkgs }:
with pkgs; [
  pinentry_mac
  terminal-notifier

  # for emacs
  emacs-git
  mu
  emacsPackages.mu4e
]
