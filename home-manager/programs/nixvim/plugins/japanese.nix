{ pkgs, sources }:
{
  # denops, skkeleton, vimdoc-ja, vim-manpager
  extraPlugins = [
    pkgs.vimPlugins.denops-vim
    (pkgs.vimUtils.buildVimPlugin {
      pname = sources.vim-skkeleton.pname;
      version = sources.vim-skkeleton.date;
      src = sources.vim-skkeleton.src;
    })
    (pkgs.vimUtils.buildVimPlugin {
      pname = sources.vim-skkeleton-azik.pname;
      version = sources.vim-skkeleton-azik.date;
      src = sources.vim-skkeleton-azik.src;
    })
    (pkgs.vimUtils.buildVimPlugin {
      pname = sources.vimdoc-ja.pname;
      version = sources.vimdoc-ja.date;
      src = sources.vimdoc-ja.src;
    })
    pkgs.vimPlugins.vim-manpager
  ];

  # skkeleton設定
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
