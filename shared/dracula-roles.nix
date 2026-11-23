# Dracula's color roles filled from the base16 palette, so apps themed with Dracula's role layout
# (fish, fzf, tmux-dracula, dracula.nvim) take their colors from Stylix. nix-on-droid has no Stylix
# and reads the same scheme from shared/stylix.nix. Values are bare hex without "#".
{
  config,
  options,
  pkgs,
}:
let
  c =
    if options ? stylix then
      config.lib.stylix.colors
    else
      (import ./stylix.nix { inherit pkgs; }).base16Scheme;
in
{
  background = c.base00;
  surface = c.base01;
  selection = c.base02;
  comment = c.base03;
  muted = c.base04;
  foreground = c.base05;
  brightWhite = c.base07;
  red = c.base08;
  orange = c.base09;
  yellow = c.base0A;
  green = c.base0B;
  cyan = c.base0C;
  blue = c.base0D;
  pink = c.base0E;
  purple = c.base0F;
}
