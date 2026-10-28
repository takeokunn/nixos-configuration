{ pkgs, ... }:
{
  # ISUCON tooling. To remove: delete this directory and the `./isucon` import in
  # ../basic.nix. General Go tooling (go, gopls, ...) lives in ../basic.nix and stays.
  home.packages = with pkgs; [
    alp
    percona-toolkit
    mariadb.client
    shellcheck
    yq-go
    go-task
    graphviz
    sqlite
    openssl
    (python3.withPackages (ps: [
      ps.pyyaml
      ps.matplotlib
    ]))
    gotestsum
  ];
}
