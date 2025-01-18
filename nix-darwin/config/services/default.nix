let
  aerospace = import ./aerospace;
  nix-daemon = import ./nix-daemon;
  sketchybar = import ./sketchybar;
in
{
  imports = [
    aerospace
    nix-daemon
    sketchybar
  ];
}
