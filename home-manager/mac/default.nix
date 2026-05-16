{ pkgs }:
let
  sketchybar = import ./sketchybar { inherit pkgs; };
in
[
  sketchybar
  {
    home.packages = with pkgs; [
      brewCasks.keycastr
      raycast
      brewCasks.docker-desktop
      postman
      brewCasks.postico
      brewCasks.sequel-ace
      brewCasks.sublime-text
      brewCasks.ngrok
      brewCasks.clickup
      brewCasks.slite
      brewCasks.element
    ];
  }
]
