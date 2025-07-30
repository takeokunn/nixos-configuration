{
  nix = {
    optimise.automatic = true;
    settings = {
      experimental-features = "nix-command flakes";
      max-jobs = 8;
      download-buffer-size = 268435456;
    };
    extraOptions = ''
      extra-substituters = https://devenv.cachix.org
      extra-trusted-public-keys = devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=
    '';
  };
}
