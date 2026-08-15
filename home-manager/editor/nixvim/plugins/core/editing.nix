{ pkgs }:
{
  plugins.lz-n.enable = true;

  plugins.undotree.enable = true;
  plugins.undotree.settings.WindowLayout = 3;

  plugins.rainbow-delimiters.enable = true;

  extraPlugins = [
    pkgs.vimPlugins.vim-table-mode
    pkgs.vimPlugins.vim-textobj-entire
  ];

  userCommands = {
    UndotreeToggleAndFocus.command = ":UndotreeToggle | :UndotreeFocus";
    UndotreeToggleAndFocus.desc = "Toggle and focus Undotree";
  };

  keymaps = [
    {
      mode = "n";
      key = "<leader>u";
      action = "<cmd>UndotreeToggleAndFocus<CR>";
    }
  ];
}
