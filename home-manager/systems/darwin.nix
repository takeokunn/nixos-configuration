{ pkgs }:
with pkgs; [
  pinentry_mac
  terminal-notifier

  # for emacs
  emacs-git
  pinentry-emacs
  emacsPackages.mu4e
]
