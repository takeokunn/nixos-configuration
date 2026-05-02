_: {
  # mini.diff and mini.git are configured in ui/mini.nix
  # Keep neogit and diffview for advanced git UI

  plugins.neogit = {
    enable = true;
    settings = {
      integrations = {
        diffview = true;
        # mini.pick integration instead of telescope
        mini_pick = true;
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
