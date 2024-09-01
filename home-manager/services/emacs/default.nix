{ pkgs, emacsPkgs }: {
  services.emacs = {
    # enable = pkgs.stdenv.isLinux;
    enable = false;
    package = emacsPkgs;
    defaultEditor = true;
  };
}
