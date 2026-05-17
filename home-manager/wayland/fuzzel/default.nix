{
  programs.fuzzel.enable = true;
  programs.fuzzel.settings = {
    main = {
      font = "HackGen Console NF:size=12";
      terminal = "kitty";
      layer = "overlay";
      prompt = "❯ ";
      icon-theme = "Papirus-Dark";
      icons-enabled = true;
      fields = "name,generic,comment,categories,filename,keywords";
      show-actions = false;
      lines = 12;
      width = 40;
      horizontal-pad = 16;
      vertical-pad = 12;
      inner-pad = 8;
    };

    # Vi-style keybindings
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

    # Dracula color scheme
    colors = {
      background = "282a36ee";
      text = "f8f8f2ff";
      prompt = "bd93f9ff";
      placeholder = "6272a4ff";
      input = "f8f8f2ff";
      match = "ff79c6ff";
      selection = "44475aff";
      selection-text = "f8f8f2ff";
      selection-match = "ff79c6ff";
      border = "bd93f9ff";
    };

    border = {
      width = 2;
      radius = 8;
    };
  };
}
