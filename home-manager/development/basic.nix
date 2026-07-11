{ nurPkgs, ... }:
{
  imports = [
    ./modules/doggo
    ./modules/lnav
    ./cargo
  ];

  home.packages = [
    nurPkgs.devenv
    nurPkgs.kuro
    nurPkgs.paredit-cli
  ];
}
