{ pkgs }:
{
  # Undotree - no mini.nvim equivalent, keep it
  plugins.undotree = {
    enable = true;
    settings = {
      WindowLayout = 3;
    };
  };

  # Rainbow delimiters - no mini.nvim equivalent, keep it
  plugins.rainbow-delimiters = {
    enable = true;
  };

  extraPlugins = with pkgs.vimPlugins; [
    vim-table-mode
    vim-textobj-entire
  ];

  userCommands = {
    UndotreeToggleAndFocus = {
      command = ":UndotreeToggle | :UndotreeFocus";
      desc = "Toggle and focus Undotree";
    };
  };

  keymaps = [
    # undotree
    {
      mode = "n";
      key = "<leader>u";
      action = "<cmd>UndotreeToggleAndFocus<CR>";
    }
  ];
}
