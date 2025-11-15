{ pkgs }:
{
  plugins.treesitter = {
    enable = true;
    settings = {
      highlight = {
        enable = true;
      };
      indent = {
        enable = true;
      };
    };
    grammarPackages = pkgs.vimPlugins.nvim-treesitter.allGrammars;
  };
}
