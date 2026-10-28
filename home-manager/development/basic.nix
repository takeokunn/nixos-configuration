{
  lib,
  nurPkgs,
  pkgs,
  ...
}:
{
  imports = [
    ./modules/doggo
    ./modules/lnav
    ./cargo
    ./isucon
  ];

  # aitools is null on systems nur-packages' upstream flake doesn't build it for
  # (currently anything but x86_64-linux/aarch64-darwin), e.g. OPPO-A79's aarch64-linux.
  home.packages = [
    nurPkgs.devenv
    nurPkgs.kuro
    nurPkgs.paredit-cli
  ]
  ++ (with pkgs; [
    go
    gopls
    gotools # goimports, used by emacs gofmt-command
    delve
    golangci-lint
    go-tools # staticcheck
    gofumpt
    gomodifytags
    gotests
    impl
  ])
  ++ lib.optional (nurPkgs.aitools != null) nurPkgs.aitools;
}
