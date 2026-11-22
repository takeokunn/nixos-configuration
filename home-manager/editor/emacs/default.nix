{
  config,
  lib,
  pkgs,
  emacsPkg,
  org-babel,
  ...
}:
let
  tangle = org-babel.lib.tangleOrgBabel { languages = [ "emacs-lisp" ]; };
  inherit (config.stylix) fonts;
in
{
  xdg.desktopEntries.emacsclient-gui = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    name = "Emacs (Client)";
    genericName = "Text Editor";
    exec = "emacsclient -c -a emacs %F";
    icon = "emacs";
    type = "Application";
    categories = [
      "Development"
      "TextEditor"
    ];
    mimeType = [
      "text/plain"
      "text/x-makefile"
    ];
    startupNotify = true;
  };

  home.sessionVariables.EDITOR = lib.mkForce "emacsclient -t";
  home.sessionVariables.VISUAL = lib.mkForce "emacsclient -c";

  home.file.".emacs.d/init.el".text = tangle (builtins.readFile ./elisp/init.org);
  home.file.".emacs.d/early-init.el".text = tangle (builtins.readFile ./elisp/early-init.org);
  # Loaded by init.el's fontset section; :height is in 1/10 pt.
  home.file.".emacs.d/stylix-fonts.el".text = ''
    ;;; stylix-fonts.el --- Fonts from Stylix -*- lexical-binding: t; -*-
    (defconst my/stylix-monospace-family ${builtins.toJSON fonts.monospace.name})
    (defconst my/stylix-monospace-height ${toString (builtins.floor (fonts.sizes.terminal * 10))})
  '';
  home.file.".emacs.d/misc/yasnippet.org".source = ./misc/yasnippet.org;
  home.file.".emacs.d/misc/mu4e-dashboard.org" = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
    source = ./misc/mu4e-dashboard.org;
  };

  home.packages = [
    emacsPkg
  ]
  ++ (with pkgs; [
    (emacs-lsp-booster.override { emacs = emacs-unstable; })
    pinentry-emacs
  ]);
}
