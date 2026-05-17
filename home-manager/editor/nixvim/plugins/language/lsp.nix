{
  # mini.completion is configured in ui/mini.nix
  # Just keep LSP and lspsaga here

  plugins.lsp.enable = true;
  plugins.lsp.keymaps.lspBuf.gd = "definition";
  plugins.lsp.servers.nil_ls.enable = true;
  plugins.lsp.servers.nil_ls.settings.formatting.command = [ "nixfmt" ];
  plugins.lsp.servers.nil_ls.settings.nix.flake.autoArchive = true;
  plugins.lsp.servers.nil_ls.settings.nix.flake.autoEvalInputs = true;
  plugins.lsp.servers.nil_ls.settings.nix.flake.nixpkgsInputName = "nixpkgs";
  plugins.lsp.servers.ts_ls.enable = true;
  plugins.lsp.servers.intelephense.enable = true;
  plugins.lsp.servers.intelephense.package = null;
  plugins.lsp.servers.gopls.enable = true;
  plugins.lsp.servers.html.enable = true;
  plugins.lsp.servers.cssls.enable = true;
  plugins.lsp.servers.bashls.enable = true;
  plugins.lsp.servers.emmet_ls.enable = true;
  plugins.lsp.servers.emmet_ls.filetypes = [
    "html"
    "css"
    "scss"
    "javascript"
    "javascriptreact"
    "typescript"
    "typescriptreact"
  ];

  plugins.lspsaga.enable = true;
  plugins.lspsaga.lazyLoad.settings.event = "LspAttach";

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
