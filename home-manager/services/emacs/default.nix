{ pkgs, emacsPkgs }: {
  services.emacs = {
    enable = pkgs.stdenv.isLinux;
    package = emacsPkgs;
    defaultEditor = true;
  };
}
