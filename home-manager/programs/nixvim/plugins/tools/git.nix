{ pkgs, customPackages }:
{
  plugins.gitsigns = {
    enable = true;
    settings = {
    };
  };

  plugins.neogit = {
    enable = true;
    settings = {
      integrations = {
        diffview = true;
        telescope = true;
      };
    };
  };

  plugins.diffview = {
    enable = true;
    lazyLoad.settings.cmd = [
      "DiffviewOpen"
      "DiffviewFileHistory"
    ];
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>gg";
      action = "<cmd>Neogit<CR>";
      options.desc = "Open Neogit";
    }
    {
      mode = "n";
      key = "<leader>gd";
      action = "<cmd>DiffviewOpen<CR>";
      options.desc = "Open Diffview";
    }
    {
      mode = "n";
      key = "<leader>gh";
      action = "<cmd>DiffviewFileHistory %<CR>";
      options.desc = "File History";
    }
  ];
}
