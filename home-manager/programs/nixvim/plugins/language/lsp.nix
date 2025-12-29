{ pkgs }:
{
  # mini.completion is configured in ui/mini.nix
  # Just keep LSP and lspsaga here

  plugins.lsp = {
    enable = true;
    keymaps = {
      lspBuf = {
        gd = "definition";
      };
    };
    servers = {
      nil_ls = {
        enable = true;
        settings = {
          formatting = {
            command = [ "nixfmt" ];
          };
          nix = {
            flake = {
              autoArchive = true;
              autoEvalInputs = true;
              nixpkgsInputName = "nixpkgs";
            };
          };
        };
      };
      ts_ls = {
        enable = true;
      };
      intelephense = {
        enable = true;
        package = null;
      };
      gopls = {
        enable = true;
      };
      html = {
        enable = true;
      };
      cssls = {
        enable = true;
      };
      bashls = {
        enable = true;
      };
      emmet_ls = {
        enable = true;
        filetypes = [
          "html"
          "css"
          "scss"
          "javascript"
          "javascriptreact"
          "typescript"
          "typescriptreact"
        ];
      };
    };
  };

  plugins.lspsaga = {
    enable = true;
    lazyLoad.settings = {
      event = "LspAttach";
    };
  };

  extraConfigLua = ''
    vim.diagnostic.config({
      virtual_text = false
    })
  '';

  keymaps = [
    {
      mode = "n";
      key = "K";
      action = "<cmd>Lspsaga hover_doc<CR>";
    }
    {
      mode = [
        "n"
        "i"
      ];
      key = "<S-M-r>";
      action = "<cmd>Lspsaga rename<CR>";
    }
    {
      mode = "n";
      key = "<M-d>";
      action = "<cmd>Lspsaga finder def+ref<CR>";
    }
    {
      mode = "n";
      key = "<M-r>";
      action = "<cmd>Lspsaga peek_definition<CR>";
    }
    {
      mode = "n";
      key = "<M-j>";
      action = "<cmd>Lspsaga diagnostic_jump_next<CR>";
    }
    {
      mode = "n";
      key = "<M-k>";
      action = "<cmd>Lspsaga diagnostic_jump_prev<CR>";
    }
  ];
}
