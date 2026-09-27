{ lib, nurPkgs, ... }:
{
  imports = [
    ./modules/doggo
    ./modules/lnav
    ./cargo
  ];

  # aitools is null on systems nur-packages' upstream flake doesn't build it for
  # (currently anything but x86_64-linux/aarch64-darwin), e.g. OPPO-A79's aarch64-linux.
  home.packages = [
    nurPkgs.devenv
    nurPkgs.kuro
    nurPkgs.paredit-cli
  ]
  ++ lib.optional (nurPkgs.aitools != null) nurPkgs.aitools;
}
