{ config, pkgs, ... }:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  services.swayosd.enable = true;
  services.swayosd.topMargin = 0.95;
  services.swayosd.stylePath = pkgs.writeText "swayosd-style.css" ''
    window {
      background: alpha(${colors.base00}, 0.9);
      border-radius: 12px;
      border: 2px solid ${colors.base0F};
      padding: 12px 20px;
    }

    #container {
      margin: 16px;
    }

    image, label {
      color: ${colors.base05};
    }

    progressbar {
      min-height: 8px;
      border-radius: 4px;
      background: ${colors.base02};
    }

    progressbar:disabled {
      background: ${colors.base02};
    }

    progressbar progress {
      min-height: 8px;
      border-radius: 4px;
      background: ${colors.base0F};
    }

    progressbar.volume.muted progress {
      background: ${colors.base08};
    }

    progressbar.brightness progress {
      background: ${colors.base0A};
    }

    .capslock image {
      color: ${colors.base0B};
    }
  '';
}
