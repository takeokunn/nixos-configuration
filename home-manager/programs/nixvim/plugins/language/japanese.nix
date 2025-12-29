{ pkgs, customPackages }:
{
  extraPlugins = [
    # Japanese documentation
    pkgs.vimPlugins.vim-manpager
    customPackages.vimdoc-ja
    # Skkeleton (Japanese IME)
    pkgs.vimPlugins.denops-vim
    customPackages.skkeleton
    customPackages.skkeleton-azik
  ];

  keymaps = [
    {
      mode = [
        "i"
        "c"
      ];
      key = "<C-j>";
      action = "<Plug>(skkeleton-toggle)";
      options.silent = true;
    }
  ];

  extraConfigLua = ''
    -- Phase 1: Initialize skkeleton WITHOUT kanaTable (table not yet registered)
    vim.fn['skkeleton#config']({
      eggLikeNewline = true,
      keepState = true,
      sources = { "skk_server" }
    })

    -- Phase 2: Register the azik kana table
    vim.fn['skkeleton#azik#add_table']('us')

    -- Phase 3: Set kanaTable AFTER azik is registered
    vim.fn['skkeleton#config']({
      kanaTable = 'azik'
    })

    -- Phase 4: Register custom kana mappings
    vim.fn['skkeleton#register_kanatable']("azik", {
      ss = { "せい" }
    })
  '';
}
