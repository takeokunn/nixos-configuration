{ pkgs }:
{
  plugins.which-key = {
    enable = true;
  };

  plugins.trouble = {
    enable = true;
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
