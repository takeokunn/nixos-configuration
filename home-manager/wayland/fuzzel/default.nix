{
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
      radius = 8;
    };
  };
}
