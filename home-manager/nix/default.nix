let
  nix-index = import ./nix-index;
  nix-init = import ./nix-init;
  nix-gc = import ./nix-gc;
in
[
  nix-index
  nix-init
  nix-gc
]
