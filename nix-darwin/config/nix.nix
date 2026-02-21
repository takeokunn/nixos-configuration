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
      trusted-users = [
        "root"
        "take"
      ];
      sandbox = false;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      cores = 0;
      max-jobs = "auto";
      http-connections = 50;
      download-buffer-size = 268435456;
      always-allow-substitutes = true;
      extra-substituters = [
        "https://devenv.cachix.org"
        "https://nix-community.cachix.org"
        "https://takeokunn-darwin-vz-nix.cachix.org"
      ];
      extra-trusted-public-keys = [
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "takeokunn-darwin-vz-nix.cachix.org-1:/JRjcn9UMUbE0DRyJUg7g+gq/e7QSUXxvz+FZprHIH4="
      ];
    };
  };
}
