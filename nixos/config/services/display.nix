{
  config,
  lib,
  pkgs,
  utils,
  username,
  ...
}:
let
  colors = config.lib.stylix.colors;
  inherit (import ../../../shared/design.nix config.stylix.fonts) font size radius;
  pt = n: "${toString n}pt";
  xkb = config.services.xserver.xkb;
  span =
    attrs: text:
    "<span ${lib.concatStringsSep " " (lib.mapAttrsToList (k: v: "${k}='${v}'") attrs)}>${text}</span>";
  hour = span { foreground = "#${colors.base0E}"; } "%H";
  minute = span { foreground = "#${colors.base0C}"; } "%M";

  # greetd builds the greeter's environment from PAM alone and neither cage nor regreet takes a
  # keyboard layout, so pam_env hands the layout to cage's libxkbcommon as XKB_DEFAULT_*.
  greeterEnvironment = pkgs.writeText "greetd-greeter-environment" ''
    XKB_DEFAULT_LAYOUT DEFAULT="${xkb.layout}"
    XKB_DEFAULT_OPTIONS DEFAULT="${xkb.options}"
  '';
  includeGreetd = {
    name = "greetd";
    control = "include";
    modulePath = "greetd";
  };
in
{
  # greetd runs regreet inside cage; regreet lists the sessions in XDG_DATA_DIRS, where
  # programs.niri installs niri.desktop (Exec=niri-session).
  services.displayManager.regreet = {
    enable = true;
    # Stylix sets the family; the size is the same step as the lock screen's secondary lines.
    font.size = size.title;
    # The clock label is Pango markup, so hours and minutes can take the lock screen's two colors.
    # regreet centers it at the top edge, so it is one line there instead of hyprlock's two.
    settings.widget.clock = {
      format = lib.concatStringsSep "\n" [
        (span {
          font_family = font.numeric;
          size = pt size.loginClock;
          weight = "bold";
        } "${hour} ${minute}")
        (span {
          size = pt size.heading;
          foreground = "#${colors.base05}";
        } "%-m月%-d日 %A")
        (span {
          size = pt size.title;
          foreground = "#${colors.base04}";
        } "${config.networking.hostName} · ${username}")
      ];
      locale = lib.head (lib.splitString "." config.i18n.extraLocaleSettings.LC_TIME);
    };
  };

  stylix.targets.regreet = {
    enable = true;
    # regreet sets the alignment of its widgets in code, so CSS only restyles them in place.
    extraCss = ''
      overlay > picture {
        filter: blur(8px) brightness(0.6);
      }
      overlay > frame.background {
        background-color: alpha(#${colors.base00}, 0.6);
        border: none;
        border-radius: ${toString radius.window}px;
      }
      overlay > frame.background entry {
        border-radius: 9999px;
      }
    '';
  };

  # Without this service greetd runs the greeter under "greetd" too; a separate one keeps the
  # XKB variables out of logged-in sessions.
  security.pam.services.greetd-greeter = {
    useDefaultRules = false;
    rules =
      lib.genAttrs [ "auth" "account" "password" ] (_: utils.pam.autoOrderRules [ includeGreetd ])
      // {
        session = utils.pam.autoOrderRules [
          includeGreetd
          {
            name = "xkb";
            control = "required";
            modulePath = "${config.security.pam.package}/lib/security/pam_env.so";
            settings = {
              conffile = "${greeterEnvironment}";
              readenv = 0;
            };
          }
        ];
      };
  };

  # Read by the console (TTY and the initrd PIN prompt) and the greeter; niri carries its own copy.
  services.xserver.xkb.layout = "jp";
  services.xserver.xkb.options = "ctrl:nocaps";
}
