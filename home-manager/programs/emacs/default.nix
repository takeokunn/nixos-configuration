{ pkgs }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-git;
  };

  home.packages = with pkgs; [
    tree-sitter
    (tree-sitter.withPlugins (p: builtins.attrValues p))
    emacs-lsp-booster
    pinentry-emacs
    mu
    emacsPackages.mu4e
  ];

  programs.fish = {
    shellAliases = { emacs = "emacs -nw"; };

    interactiveShellInit = ''
      set -gx LSP_USE_PLISTS true
      set -gx EDITOR "emacs -nw"
      set -gx HOMEBREW_EDITOR "emacs -nw"
    '';
  };
}
