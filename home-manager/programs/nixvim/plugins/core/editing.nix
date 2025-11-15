{ pkgs }:
{
  plugins.hop = {
    enable = true;
  };

  keymaps = [
    {
      mode = "n";
      key = "f";
      action = "<Cmd>HopChar1<CR>";
    }
  ];

  plugins.undotree = {
    enable = true;
    settings = {
      WindowLayout = 3;
    };
  };

  extraPlugins = with pkgs.vimPlugins; [
    vim-bracketed-paste
    vim-table-mode
    vim-textobj-entire
    auto-pairs
    rainbow
    vim-sandwich
  ];

  extraConfigLua = ''
    vim.api.nvim_create_user_command('UndotreeToggleAndFocus', ':UndotreeToggle | :UndotreeFocus', {})
    vim.keymap.set('n', '<leader>u', vim.cmd.UndotreeToggleAndFocus)

    vim.g.rainbow_active = 1

    vim.call('operator#sandwich#set', 'add', 'char', 'skip_space', 1)
  '';
}
