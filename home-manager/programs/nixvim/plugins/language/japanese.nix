{ pkgs, customPlugins }:
{
  extraPlugins = [
    pkgs.vimPlugins.denops-vim
    customPlugins.skkeleton
    customPlugins.skkeleton-azik
    customPlugins.vimdoc-ja
    pkgs.vimPlugins.vim-manpager
  ];

  extraConfigLua = ''
    vim.fn['skkeleton#config']({
      eggLikeNewline = true,
      keepState = true,
      sources = { "skk_server" }
    })
    vim.keymap.set({ 'i', 'c' }, '<C-j>', '<Plug>(skkeleton-toggle)', { silent = true })

    vim.fn['skkeleton#azik#add_table']('us')
    vim.fn['skkeleton#config']({
      kanaTable = 'azik'
    })
    vim.call("skkeleton#register_kanatable", "azik", {
      ss = { "せい" },
    })
  '';
}
