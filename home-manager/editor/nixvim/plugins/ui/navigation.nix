{
  plugins.trouble.enable = true;
  plugins.trouble.settings.keys."<Esc>" = "close";

  plugins.aerial.enable = true;
  plugins.aerial.settings.backends = [
    "lsp"
    "treesitter"
  ];
  plugins.aerial.settings.layout.default_direction = "right";
  plugins.aerial.settings.layout.placement = "edge";
  plugins.aerial.settings.keymaps."<Esc>" = "actions.close";
  plugins.aerial.lazyLoad.settings.cmd = "AerialToggle";

  keymaps = [
    {
      mode = "n";
      key = "<leader>a";
      action = "<cmd>AerialToggle<CR>";
      options.desc = "Toggle Aerial";
    }
  ];
}
