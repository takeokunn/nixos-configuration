{
  plugins.neogit.enable = true;
  plugins.neogit.settings.integrations.diffview = true;
  plugins.neogit.settings.integrations.mini_pick = true;

  plugins.diffview.enable = true;
  plugins.diffview.lazyLoad.settings.cmd = [
    "DiffviewOpen"
    "DiffviewFileHistory"
  ];

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
