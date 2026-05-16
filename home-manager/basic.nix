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

  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
  };
  shell = import ./shell/basic.nix { inherit pkgs nurPkgs; };
  editor = import ./editor/basic.nix { inherit pkgs nurPkgs; };
  vcs = import ./vcs/basic.nix { inherit pkgs nurPkgs; };
  security = import ./security/basic.nix { inherit pkgs; };
  development = import ./development/basic.nix { inherit pkgs devenvPkgs; };
in
{
  imports = shell ++ editor ++ vcs ++ security ++ development;
  home.stateVersion = "25.11";
  home.enableNixpkgsReleaseCheck = false;
}
