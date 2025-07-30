{
  programs.sops.enable = true;

  programs.fish = {
    shellInit = ''
      set -x SOPS_PGP_FP 0F79C0AB03FD7A1C
    '';
  };
}
