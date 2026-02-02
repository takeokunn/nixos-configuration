{
  system,
  nixpkgs,
  nur-packages,
  ...
}:
let
  # nur-packages
  nurPkgs = nur-packages.packages.${system};

  # packages
  basicOverlay = import ./overlay/basic.nix;
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = basicOverlay;
  };
  basicPkgs = import ./packages/basic.nix { inherit pkgs; };

  # modules
  modules = import ./modules;

  # programs
  basicPrograms = import ./programs/basic.nix {
    inherit pkgs nurPkgs;
  };

  # services
  basicServices = import ./services/basic.nix;
in
{
  imports = modules ++ basicPrograms ++ basicServices;
  home.stateVersion = "24.11";
  home.enableNixpkgsReleaseCheck = false;
  home.packages = basicPkgs;
}
