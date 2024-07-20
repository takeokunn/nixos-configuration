{ pkgs }:
with pkgs; [
  pinentry_mac
  terminal-notifier

  # for emacs
  mu
  emacs-git
  pinentry-emacs
  emacsPackages.mu4e
]
