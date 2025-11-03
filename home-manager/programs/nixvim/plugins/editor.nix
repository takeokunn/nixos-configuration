{ pkgs, sources }:
{
  # hop (cursor movement)
  plugins.hop = {
    enable = true;
  };

  # hop keymap
  keymaps = [
    {
      mode = "n";
      key = "f";
      action = "<Cmd>HopChar1<CR>";
    }
  ];

  # undotree
  plugins.undotree = {
    enable = true;
    settings = {
      WindowLayout = 3;
    };
  };

  # vim-fern (カスタムビルド) とその他のプラグイン
  extraPlugins = [
    (pkgs.vimUtils.buildVimPlugin {
      pname = sources.vim-fern.pname;
      version = sources.vim-fern.date;
      src = sources.vim-fern.src;
    })
    (pkgs.vimUtils.buildVimPlugin {
      pname = sources.vim-nerdfont.pname;
      version = sources.vim-nerdfont.date;
      src = sources.vim-nerdfont.src;
    })
    (pkgs.vimUtils.buildVimPlugin {
      pname = sources.vim-fern-renderer-nerdfont.pname;
      version = sources.vim-fern-renderer-nerdfont.date;
      src = sources.vim-fern-renderer-nerdfont.src;
    })
    pkgs.vimPlugins.vim-bracketed-paste
    pkgs.vimPlugins.vim-table-mode
    pkgs.vimPlugins.vim-textobj-entire
    pkgs.vimPlugins.auto-pairs
    pkgs.vimPlugins.rainbow
    pkgs.vimPlugins.vim-sandwich
    pkgs.vimPlugins.editorconfig-nvim
    pkgs.vimPlugins.vim-suda
    pkgs.vimPlugins.goyo-vim
    pkgs.vimPlugins.vim-gnupg
  ];

  # undotree, fern, rainbow, vim-sandwich設定
  extraConfigLua = ''
    vim.api.nvim_create_user_command('UndotreeToggleAndFocus', ':UndotreeToggle | :UndotreeFocus', {})
    vim.keymap.set('n', '<leader>u', vim.cmd.UndotreeToggleAndFocus)

    vim.g["fern#drawer_width"] = 50
    vim.keymap.set('n', '<leader>e', '<cmd>Fern . -right -drawer<cr>')
    vim.g["fern#renderer"] = "nerdfont"

    vim.g.rainbow_active = 1

    vim.call('operator#sandwich#set', 'add', 'char', 'skip_space', 1)
  '';
}
