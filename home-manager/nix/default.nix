let
  nix-index = import ./nix-index;
  nix-init = import ./nix-init;
  nix-config = import ./nix-config;
  nix-gc = import ./nix-gc;
in
[
  nix-index
  nix-init
  nix-config
  nix-gc
]
