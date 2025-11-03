{
  keymaps = [
    # basic keymap
    {
      mode = "n";
      key = "/";
      action = "/\\v";
      options.noremap = true;
    }
    {
      mode = "n";
      key = "U";
      action = "<C-r>";
      options.noremap = true;
    }
    {
      mode = "n";
      key = "<Leader><Leader>";
      action = "V";
      options.noremap = true;
    }
    {
      mode = "n";
      key = "<Esc><Esc>";
      action = "<Cmd>nohlsearch<CR><Esc>";
      options.noremap = true;
    }
    {
      mode = "i";
      key = "<C-j>";
      action = "<CR>";
    }

    # window keymap
    {
      mode = "n";
      key = "sj";
      action = "<C-w>j";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = "n";
      key = "sk";
      action = "<C-w>k";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = "n";
      key = "sl";
      action = "<C-w>l";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = "n";
      key = "sh";
      action = "<C-w>h";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = "n";
      key = "sJ";
      action = "<C-w>J";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = "n";
      key = "sK";
      action = "<C-w>K";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = "n";
      key = "sL";
      action = "<C-w>L";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = "n";
      key = "sH";
      action = "<C-w>H";
      options = {
        noremap = true;
        silent = true;
      };
    }
    {
      mode = "n";
      key = "sw";
      action = "<C-w>w";
      options = {
        noremap = true;
        silent = true;
      };
    }
  ];
}
