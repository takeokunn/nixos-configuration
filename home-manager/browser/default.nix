{ pkgs, firefox-addons }:
let
  firefox = import ./firefox { inherit pkgs; };
  chromium = import ./chromium { inherit pkgs; };
  # zen-browser = import ./zen-browser { inherit pkgs firefox-addons; };  # TODO: hash mismatch, re-enable after fix
in
[ firefox chromium ]
