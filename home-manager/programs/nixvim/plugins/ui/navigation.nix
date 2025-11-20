{ pkgs }:
{
  plugins.which-key = {
    enable = true;
  };

  plugins.trouble = {
    enable = true;
    settings = {
      keys = {
        "<Esc>" = "close";
      };
    };
  };

  plugins.aerial = {
    enable = true;
    settings = {
      backends = [
        "lsp"
        "treesitter"
      ];
      layout = {
        default_direction = "right";
        placement = "edge";
      };
      keymaps = {
        "<Esc>" = "actions.close";
      };
    };
    lazyLoad.settings.cmd = "AerialToggle";
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>a";
      action = "<cmd>AerialToggle<CR>";
      options.desc = "Toggle Aerial";
    }
  ];
}
