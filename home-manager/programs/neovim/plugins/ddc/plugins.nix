{ vimUtils, sources }:
{
  ddc-source-file = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-file.pname;
    version = sources.vim-ddc-source-file.date;
    src = sources.vim-ddc-source-file.src;
  };
  ddc-converter_remove_overlap = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-converter_remove_overlap.pname;
    version = sources.vim-ddc-converter_remove_overlap.date;
    src = sources.vim-ddc-converter_remove_overlap.src;
  };
  ddc-matcher_head = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-matcher_head.pname;
    version = sources.vim-ddc-matcher_head.date;
    src = sources.vim-ddc-matcher_head.src;
  };
  ddc-matcher_length = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-matcher_length.pname;
    version = sources.vim-ddc-matcher_length.date;
    src = sources.vim-ddc-matcher_length.src;
  };
  ddc-sorter_rank = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-sorter_rank.pname;
    version = sources.vim-ddc-sorter_rank.date;
    src = sources.vim-ddc-sorter_rank.src;
  };
  ddc-source-around = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-around.pname;
    version = sources.vim-ddc-source-around.date;
    src = sources.vim-ddc-source-around.src;
  };
  ddc-source-cmdline = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-cmdline.pname;
    version = sources.vim-ddc-source-cmdline.date;
    src = sources.vim-ddc-source-cmdline.src;
  };
  ddc-source-cmdline-history = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-cmdline-history.pname;
    version = sources.vim-ddc-source-cmdline-history.date;
    src = sources.vim-ddc-source-cmdline-history.src;
  };
  ddc-source-codeium = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-codeium.pname;
    version = sources.vim-ddc-source-codeium.date;
    src = sources.vim-ddc-source-codeium.src;
  };
  ddc-source-input = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-input.pname;
    version = sources.vim-ddc-source-input.date;
    src = sources.vim-ddc-source-input.src;
  };
  ddc-source-line = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-line.pname;
    version = sources.vim-ddc-source-line.date;
    src = sources.vim-ddc-source-line.src;
  };
  ddc-source-lsp = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-lsp.pname;
    version = sources.vim-ddc-source-lsp.date;
    src = sources.vim-ddc-source-lsp.src;
  };
  ddc-source-rg = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-rg.pname;
    version = sources.vim-ddc-source-rg.date;
    src = sources.vim-ddc-source-rg.src;
  };
  ddc-source-shell-native = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-shell-native.pname;
    version = sources.vim-ddc-source-shell-native.date;
    src = sources.vim-ddc-source-shell-native.src;
  };
  ddc-ui-pum = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-ui-pum.pname;
    version = sources.vim-ddc-ui-pum.date;
    src = sources.vim-ddc-ui-pum.src;
  };
  ddc = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc.pname;
    version = sources.vim-ddc.date;
    src = sources.vim-ddc.src;
  };
  ddc-shell-history = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-shell-history.pname;
    version = sources.vim-ddc-shell-history.date;
    src = sources.vim-ddc-shell-history.src;
  };
  ddc-buffer = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-buffer.pname;
    version = sources.vim-ddc-buffer.date;
    src = sources.vim-ddc-buffer.src;
  };
  ddc-fuzzy = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-fuzzy.pname;
    version = sources.vim-ddc-fuzzy.date;
    src = sources.vim-ddc-fuzzy.src;
  };
  ddc-source-lsp-setup = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-lsp-setup.pname;
    version = sources.vim-ddc-source-lsp-setup.date;
    src = sources.vim-ddc-source-lsp-setup.src;
  };
  ddc-source-nvim-lua = vimUtils.buildVimPlugin {
    pname = sources.vim-ddc-source-nvim-lua.pname;
    version = sources.vim-ddc-source-nvim-lua.date;
    src = sources.vim-ddc-source-nvim-lua.src;
  };
}
