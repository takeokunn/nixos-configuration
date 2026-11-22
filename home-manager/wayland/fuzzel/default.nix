{ config, pkgs, ... }:
let
  inherit (import ../../../shared/design.nix config.stylix.fonts) radius;
  systemctl = "${pkgs.systemd}/bin/systemctl";

  # Opened by Mod+Shift+E and the rail's power button. Each row is "label\0icon\x1f<name>"
  # (fuzzel prints only the label back); the icon names come from the Dracula theme Stylix
  # gives fuzzel. Anything typed that matches no row falls through and does nothing.
  powerMenu = pkgs.writeShellScriptBin "power-menu" ''
    choice="$(
      printf '%s\0icon\x1f%s\n' \
        Lock system-lock-screen \
        Suspend system-suspend \
        Hibernate system-hibernate \
        Reboot system-reboot \
        "Power off" system-shutdown \
        "Log out" system-log-out |
        ${config.programs.fuzzel.package}/bin/fuzzel --dmenu --lines 6 --width 20
    )" || exit 0

    case "$choice" in
      Lock)
        # Same guard as hypridle's lock_cmd, so a second hyprlock never stacks on the first.
        ${pkgs.procps}/bin/pgrep -x hyprlock >/dev/null ||
          exec ${config.programs.hyprlock.package}/bin/hyprlock
        ;;
      Suspend) exec ${systemctl} suspend ;;
      Hibernate) exec ${systemctl} hibernate ;;
      Reboot) exec ${systemctl} reboot ;;
      "Power off") exec ${systemctl} poweroff ;;
      "Log out") exec ${config.wayland.windowManager.niri.package}/bin/niri msg action quit --skip-confirmation ;;
    esac
  '';
in
{
  home.packages = [ powerMenu ];

  programs.fuzzel.enable = true;
  # Stylix sets the font, colors and icon theme.
  stylix.targets.fuzzel.enable = true;
  programs.fuzzel.settings = {
    main = {
      terminal = "kitty";
      layer = "overlay";
      prompt = "❯ ";
      icons-enabled = true;
      fields = "name,generic,comment,categories,filename,keywords";
      show-actions = false;
      lines = 12;
      width = 40;
      horizontal-pad = 16;
      vertical-pad = 12;
      inner-pad = 8;
    };

    key-bindings = {
      prev = "Up Control+k Control+p";
      next = "Down Control+j Control+n";
      first = "Control+g";
      last = "Control+Shift+g";
      cursor-left = "Left Control+h";
      cursor-right = "Right Control+l";
      cursor-home = "Home Control+a";
      cursor-end = "End Control+e";
      cursor-left-word = "Control+Left Control+b";
      cursor-right-word = "Control+Right Control+w";
      delete-prev = "BackSpace Control+BackSpace";
      delete-next = "Delete";
      delete-prev-word = "Control+w";
      delete-next-word = "Control+d";
      delete-line-backward = "Control+u";
      delete-line-forward = "Control+Shift+k";
      execute = "Return KP_Enter";
      execute-or-next = "Tab";
      cancel = "Escape Control+c Control+bracketleft";
    };

    border = {
      width = 2;
      radius = radius.window;
    };
  };
}
