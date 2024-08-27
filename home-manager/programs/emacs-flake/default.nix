{ lib, pkgs, emacs-flake }:
let tangle = lib.tangleOrgBabel { languages = [ "emacs-lisp" ]; };
in {
  home.file.".emacs.d/init.el".source = tangle (builtins.readFile ./index.org);
  home.file.".emacs.d/early-init.el".source =
    tangle (builtins.readFile ./early-init.org);

  programs.emacs = {
    enable = pkgs.stdenv.isLinux;
    package = emacs-flake.packages.${pkgs.system}.default;
  };

  home.packages = with pkgs; [ emacs-lsp-booster pinentry-emacs ];

  programs.fish = {
    shellAliases = { emacs = "emacs -nw"; };

    interactiveShellInit = ''
      set -gx LSP_USE_PLISTS true
      set -gx EDITOR "emacs -nw"
      set -gx HOMEBREW_EDITOR "emacs -nw"
    '';
  };
}
