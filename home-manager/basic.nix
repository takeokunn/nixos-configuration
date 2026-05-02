{
  system,
  nixpkgs,
  nur-packages,
  devenv,
  ...
}:
let
  nurPkgs = nur-packages.packages.${system};
  devenvPkgs = devenv.packages.${system};

  basicOverlay = import ./overlay/basic.nix;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = basicOverlay;
  };
  basicPkgs = import ./packages/basic.nix { inherit pkgs nurPkgs devenvPkgs; };

  shell = import ./shell/basic.nix { inherit pkgs nurPkgs; };
  editor = import ./editor/basic.nix { inherit pkgs nurPkgs; };
  vcs = import ./vcs/basic.nix { inherit nurPkgs; };
  security = import ./security/basic.nix { inherit pkgs; };
  development = import ./development/basic.nix { inherit pkgs; };
in
{
  imports = shell ++ editor ++ vcs ++ security ++ development;
  home.stateVersion = "25.11";
  home.enableNixpkgsReleaseCheck = false;
  home.packages = basicPkgs;
}
