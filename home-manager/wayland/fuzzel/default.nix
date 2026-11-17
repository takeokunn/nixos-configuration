{ config, ... }:
let
  colors = config.lib.stylix.colors;
in
{
  programs.fuzzel.enable = true;
  programs.fuzzel.settings = {
    main = {
      font = "${config.stylix.fonts.monospace.name}:size=12";
      terminal = "kitty";
      layer = "overlay";
      prompt = "❯ ";
      icon-theme = config.gtk.iconTheme.name;
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

    colors = {
      background = "${colors.base00}ee";
      text = "${colors.base05}ff";
      prompt = "${colors.base0F}ff";
      placeholder = "${colors.base03}ff";
      input = "${colors.base05}ff";
      match = "${colors.base0E}ff";
      selection = "${colors.base02}ff";
      selection-text = "${colors.base05}ff";
      selection-match = "${colors.base0E}ff";
      border = "${colors.base0F}ff";
    };

    border = {
      width = 2;
      radius = 8;
    };
  };
}
