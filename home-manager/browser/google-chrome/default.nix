{ pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  # The Mac installs Chrome from Homebrew; on Linux it comes from nixpkgs and is the default browser.
  # Extensions are installed by the NixOS policy in nixos/config/browser.nix.
  programs.google-chrome.enable = true;

  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = lib.genAttrs [
    "text/html"
    "x-scheme-handler/http"
    "x-scheme-handler/https"
    "x-scheme-handler/about"
    "x-scheme-handler/unknown"
  ] (_: "google-chrome.desktop");
}
