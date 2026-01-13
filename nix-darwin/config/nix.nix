{
  nix = {
    optimise = {
      automatic = true;
      interval = [
        {
          Hour = 3;
          Minute = 0;
        }
      ];
    };
    gc = {
      automatic = true;
      interval = [
        {
          Hour = 3;
          Minute = 30;
        }
      ];
      options = "--delete-older-than 7d";
    };
    settings = {
      sandbox = true;
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
