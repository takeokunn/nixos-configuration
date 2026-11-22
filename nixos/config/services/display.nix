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
  inherit (import ../../../shared/design.nix config.stylix.fonts)
    font
    size
    date
    radius
    ;
  pt = n: "${toString n}pt";
  px = n: "${toString n}px";
  xkb = config.services.xserver.xkb;
  span =
    attrs: text:
    "<span ${lib.concatStringsSep " " (lib.mapAttrsToList (k: v: "${k}='${v}'") attrs)}>${text}</span>";
  c = colors.withHashtag;
  hour = span { foreground = c.base0E; } "%H";
  colon = span { foreground = c.base03; } ":";
  minute = span { foreground = c.base0C; } "%M";

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
  # At boot greetd logs straight into niri, and niri locks it with hyprlock at once
  # (home-manager/wayland/niri), so the lock screen is the boot login screen; regreet appears only
  # after logging out. The disk stays behind the LUKS PIN. The marker tells niri to lock: without a
  # runtime directory to hold it, the session ends instead of starting unlocked.
  services.greetd.settings.initial_session = {
    user = username;
    command = "${pkgs.writeShellScript "niri-autologin" ''
      [ -n "''${XDG_RUNTIME_DIR:-}" ] || exit 1
      touch "$XDG_RUNTIME_DIR/lock-at-login" || exit 1
      exec /run/current-system/sw/bin/niri-session
    ''}";
  };

  # greetd runs regreet inside cage; regreet lists the sessions in XDG_DATA_DIRS, where
  # programs.niri installs niri.desktop (Exec=niri-session).
  services.displayManager.regreet = {
    enable = true;
    # Stylix sets the family; the size is the lock screen's hint size.
    font.size = size.subtitle;
    settings = {
      # Start at the password prompt for the last user and session; Cancel goes back to choosing.
      skip_selection = true;
      appearance.greeting_msg = "Touch your YubiKey or enter your password";
      # The lock screen's clock and date, on one line each since regreet centers the clock.
      widget.clock = {
        format = lib.concatStringsSep "\n" [
          (span {
            font_family = font.numeric;
            size = pt size.loginClock;
            weight = "bold";
          } "${hour}${colon}${minute}")
          (span {
            font_family = font.sans;
            size = pt size.title;
            weight = "bold";
            foreground = c.base05;
          } date.format)
        ];
        inherit (date) locale;
      };
    };
  };

  stylix.targets.regreet = {
    enable = true;
    # regreet places its widgets in code; CSS restyles them in place. The overlay holds the
    # wallpaper, the login frame, the clock frame and the power buttons, in that order. The frames
    # float without a card: a card would show the empty band the fixed 100px label column leaves.
    # The labels and the manual-entry toggles are hidden (the greeting says what to do), and the
    # login frame's right padding mirrors that label column so the fields sit on the centre line.
    extraCss = ''
      window {
        background-color: ${c.base00};
        background-image:
          radial-gradient(circle at 18% 22%, alpha(${c.base0F}, 0.32), transparent 45%),
          radial-gradient(circle at 82% 30%, alpha(${c.base0C}, 0.2), transparent 42%),
          radial-gradient(circle at 60% 88%, alpha(${c.base0E}, 0.24), transparent 48%);
      }
      overlay > picture {
        filter: blur(48px) brightness(0.5) saturate(1.3);
        opacity: 0.55;
      }
      overlay > frame.background {
        background-color: transparent;
        border: none;
        box-shadow: none;
        padding: 0 100px 0 0;
      }
      overlay > frame.background:nth-child(3) {
        margin-top: 200px;
        padding: 0;
      }
      overlay > frame.background grid > label:first-child {
        color: ${c.base04};
        font-size: ${pt size.body};
        font-weight: 500;
        margin-left: 115px;
      }
      overlay > frame.background button.toggle,
      overlay > frame.background grid > label:not(:first-child) {
        background: none;
        border: none;
        box-shadow: none;
        color: transparent;
        font-size: 1px;
        margin: 0;
        min-height: 0;
        min-width: 0;
        opacity: 0;
        padding: 0;
        -gtk-icon-size: 0;
      }
      overlay > frame.background combobox button,
      overlay > frame.background entry {
        background-color: alpha(${c.base05}, 0.05);
        background-image: none;
        border: 1px solid alpha(${c.base05}, 0.08);
        border-radius: ${px radius.chip};
        box-shadow: none;
        color: ${c.base05};
        min-height: 44px;
        outline: none;
      }
      overlay > frame.background combobox button:hover,
      overlay > frame.background entry:hover {
        background-color: alpha(${c.base05}, 0.08);
      }
      overlay > frame.background entry:focus-within {
        border-color: alpha(${c.base0E}, 0.8);
        box-shadow: 0 0 0 3px alpha(${c.base0E}, 0.2);
      }
      overlay > frame.background combobox:disabled button {
        background-color: transparent;
        border-color: transparent;
      }
      overlay > frame.background combobox:disabled button cellview {
        color: ${c.base05};
        font-weight: 600;
      }
      overlay > frame.background combobox:disabled arrow {
        opacity: 0;
      }
      /* Cancel and Login together span the field: the button row also covers the hidden toggle
         column, so Login gives that column's 15px spacing back. */
      overlay > frame.background button.suggested-action {
        margin-right: 15px;
        min-height: 44px;
        min-width: 287px;
        padding: 0 24px;
      }
      overlay > frame.background button.suggested-action label {
        color: ${c.base00};
      }
      overlay > frame.background button:not(.suggested-action):not(.toggle) {
        background: none;
        border: none;
        box-shadow: none;
        color: ${c.base04};
        min-height: 44px;
        min-width: 88px;
        padding: 0 16px;
      }
      button.suggested-action {
        background-image: linear-gradient(120deg, ${c.base0C}, ${c.base0F} 55%, ${c.base0E});
        border: none;
        border-radius: 9999px;
        box-shadow: 0 10px 30px alpha(${c.base0F}, 0.35);
        font-weight: 700;
        outline: none;
      }
      button.suggested-action:hover,
      button.suggested-action:focus-visible {
        box-shadow: 0 0 0 3px alpha(${c.base0E}, 0.35), 0 12px 36px alpha(${c.base0E}, 0.45);
      }
      button.destructive-action {
        background: none;
        border: none;
        box-shadow: none;
        color: ${c.base03};
        font-weight: 500;
        letter-spacing: 1px;
        outline: none;
      }
      button.destructive-action:hover {
        background-color: alpha(${c.base08}, 0.1);
        color: ${c.base08};
      }
      infobar > revealer > box {
        background-color: alpha(${c.base00}, 0.55);
        background-image: none;
        border: 1px solid alpha(${c.base05}, 0.08);
        border-radius: 9999px;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.35);
        padding: 0 10px;
      }
      infobar > revealer > box label {
        color: ${c.base04};
      }
      infobar.error > revealer > box {
        background-color: alpha(${c.base08}, 0.14);
        border-color: alpha(${c.base08}, 0.35);
      }
      infobar.error > revealer > box label {
        color: ${c.base08};
      }
      popover > contents {
        background-color: alpha(${c.base00}, 0.95);
        border: 1px solid alpha(${c.base05}, 0.08);
        border-radius: ${px radius.chip};
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
