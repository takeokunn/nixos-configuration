{ pkgs, config, inputs, ... }: {
  cachix.enable = false;

  packages = with pkgs; [ nixfmt-classic ];

  languages.javascript = {
    enable = true;
    package = pkgs.nodejs_21;
    npm.enable = true;
  };

  languages.nix.enable = true;
}
