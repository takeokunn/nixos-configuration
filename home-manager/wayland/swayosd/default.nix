{ config, pkgs, ... }:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  services.swayosd.enable = true;
  services.swayosd.topMargin = 0.95;
  # swayosd gives volume and brightness no CSS class of their own; mute only makes the
  # progress bar insensitive. The shadow comes from niri's layer-rule.
  services.swayosd.stylePath = pkgs.writeText "swayosd-style.css" ''
    window#osd {
      min-width: 340px;
      min-height: 50px;
      background: alpha(${colors.base00}, 0.9);
      border-radius: 25px;
      border: 2px solid ${colors.base0A};
    }

    #container {
      margin: 0 18px;
    }

    image, label {
      color: ${colors.base05};
    }

    progressbar,
    progressbar trough {
      min-height: 8px;
      border-radius: 4px;
      border: none;
    }

    progressbar trough {
      background: alpha(${colors.base05}, 0.1);
    }

    progressbar progress {
      min-height: 8px;
      border-radius: 4px;
      border: none;
      background-image: linear-gradient(to right, ${colors.base0A}, ${colors.base09});
    }

    progressbar:disabled progress {
      background-image: none;
      background-color: ${colors.base08};
    }
  '';
}
