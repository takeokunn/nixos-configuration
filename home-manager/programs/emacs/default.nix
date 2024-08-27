{ pkgs }: {
  programs.emacs = {
    enable = pkgs.stdenv.isDarwin;
    package = pkgs.emacs-git;
  };

  home.packages = with pkgs; [
    emacs-lsp-booster
    pinentry-emacs
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
