{ pkgs }:
{
  # wob - Wayland Overlay Bar (lightweight OSD)
  # No sway dependency, pure Wayland
  services.wob = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    settings = {
      "" = {
        # Dracula theme colors
        background_color = "282a36";
        border_color = "bd93f9";
        bar_color = "bd93f9";

        # Dimensions
        width = 400;
        height = 50;
        border_size = 2;
        bar_padding = 4;
        border_offset = 4;
        border_radius = 8;

        # Position (top center)
        anchor = "top";
        margin = 50;

        # Animation
        timeout = 1000;
      };
    };
  };

  home.packages = with pkgs; pkgs.lib.optionals pkgs.stdenv.isLinux [
    wob
    pamixer  # For volume control scripts
  ];
}
