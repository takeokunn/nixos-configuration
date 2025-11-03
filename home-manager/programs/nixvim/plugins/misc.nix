{ pkgs, sources }:
{
  # nvim-aibo, vim-markdown
  extraPlugins = [
    (pkgs.vimUtils.buildVimPlugin {
      pname = sources.nvim-aibo.pname;
      version = sources.nvim-aibo.version;
      src = sources.nvim-aibo.src;
      # ヘルプタグ生成エラーを回避
      dontInstallDoc = true;
      doCheck = false;
      nativeBuildInputs = [ ];
      buildInputs = [ ];
      fixupPhase = ''
        echo "Skipping help tag generation for nvim-aibo"
      '';
    })
    pkgs.vimPlugins.vim-markdown
  ];
}
