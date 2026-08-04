{ pkgs, lib, ... }:
lib.mkIf (!pkgs.stdenv.isDarwin) {
  programs.mu.enable = true;
  programs.mu.package = pkgs.mu.override { emacs = pkgs.emacs-unstable; };

  accounts.email.accounts.Gmail.mu.enable = true;
}
