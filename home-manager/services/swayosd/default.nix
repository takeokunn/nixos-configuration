{ pkgs }:
{
  services.swayosd = pkgs.lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    topMargin = 0.95;
    stylePath = pkgs.writeText "swayosd-style.css" ''
      window {
        background: alpha(#282a36, 0.9);
        border-radius: 12px;
        border: 2px solid #bd93f9;
        padding: 12px 20px;
      }

      #container {
        margin: 16px;
      }

      image, label {
        color: #f8f8f2;
      }

      progressbar {
        min-height: 8px;
        border-radius: 4px;
        background: #44475a;
      }

      progressbar:disabled {
        background: #44475a;
      }

      progressbar progress {
        min-height: 8px;
        border-radius: 4px;
        background: #bd93f9;
      }

      /* Volume muted */
      progressbar.volume.muted progress {
        background: #ff5555;
      }

      /* Brightness */
      progressbar.brightness progress {
        background: #f1fa8c;
      }

      /* Caps Lock indicator */
      .capslock image {
        color: #50fa7b;
      }
    '';
  };

  home.packages = with pkgs; pkgs.lib.optionals pkgs.stdenv.isLinux [
    swayosd
  ];
}
