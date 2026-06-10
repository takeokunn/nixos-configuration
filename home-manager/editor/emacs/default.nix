{
  lib,
  pkgs,
  emacsPkg,
  org-babel,
}:
let
  tangle = org-babel.lib.tangleOrgBabel { languages = [ "emacs-lisp" ]; };
in
{
  xdg.desktopEntries.emacsclient-gui = lib.mkIf pkgs.stdenv.isLinux {
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

  # Override vim.defaultEditor from programs/basic.nix for advanced configurations
  home.sessionVariables.EDITOR = lib.mkForce "emacsclient -t";
  home.sessionVariables.VISUAL = lib.mkForce "emacsclient -c";

  home.file.".emacs.d/init.el".text = tangle (builtins.readFile ./elisp/init.org);
  home.file.".emacs.d/early-init.el".text = tangle (builtins.readFile ./elisp/early-init.org);
  home.file.".emacs.d/misc/yasnippet.org".source = ./misc/yasnippet.org;
  home.file.".emacs.d/misc/mu4e-dashboard.org".source = ./misc/mu4e-dashboard.org;

  home.packages = [
    emacsPkg
  ]
  ++ (with pkgs; [
    (emacs-lsp-booster.override { emacs = emacs-unstable; })
    pinentry-emacs
  ]);
}
