{ pkgs }:
{
  plugins.hop = {
    enable = true;
  };

  plugins.undotree = {
    enable = true;
    settings = {
      WindowLayout = 3;
    };
  };

  plugins.vim-sandwich = {
    enable = true;
    skipSpace = true;
  };

  plugins.luasnip = {
    enable = true;
    filetypeExtend = {
      typescript = [ "javascript" ];
      typescriptreact = [
        "typescript"
        "javascript"
      ];
    };
  };

  # Lua製の自動ペアリングプラグイン
  plugins.nvim-autopairs = {
    enable = true;
    settings = {
      check_ts = true;
      ts_config = {
        lua = [
          "string"
          "source"
        ];
        javascript = [
          "string"
          "template_string"
        ];
      };
    };
  };

  # Lua製のレインボー括弧プラグイン
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
    # hop
    {
      mode = "n";
      key = "f";
      action = "<Cmd>HopChar1<CR>";
    }
    # undotree
    {
      mode = "n";
      key = "<leader>u";
      action = "<cmd>UndotreeToggleAndFocus<CR>";
    }
  ];
}
