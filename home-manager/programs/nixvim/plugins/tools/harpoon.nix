{ pkgs }:
{
  plugins.harpoon = {
    enable = true;
    lazyLoad.settings = {
      cmd = "Harpoon";
    };
    settings = {
      settings = {
        save_on_toggle = true;
      };
    };
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>ha";
      action.__raw = ''
        function()
          require("harpoon"):list():add()
        end
      '';
      options.desc = "Add file to Harpoon";
    }
    {
      mode = "n";
      key = "<leader>hh";
      action.__raw = ''
        function()
          local harpoon = require("harpoon")
          harpoon.ui:toggle_quick_menu(harpoon:list())
        end
      '';
      options.desc = "Toggle Harpoon menu";
    }
    {
      mode = "n";
      key = "<leader>1";
      action.__raw = ''
        function()
          require("harpoon"):list():select(1)
        end
      '';
      options.desc = "Harpoon file 1";
    }
    {
      mode = "n";
      key = "<leader>2";
      action.__raw = ''
        function()
          require("harpoon"):list():select(2)
        end
      '';
      options.desc = "Harpoon file 2";
    }
    {
      mode = "n";
      key = "<leader>3";
      action.__raw = ''
        function()
          require("harpoon"):list():select(3)
        end
      '';
      options.desc = "Harpoon file 3";
    }
    {
      mode = "n";
      key = "<leader>4";
      action.__raw = ''
        function()
          require("harpoon"):list():select(4)
        end
      '';
      options.desc = "Harpoon file 4";
    }
  ];
}
