let
  display = import ./display.nix;
  editing = import ./editing.nix;
  search = import ./search.nix;
  performance = import ./performance.nix;
  clipboard = import ./clipboard.nix;
  backup = import ./backup.nix;
in
display // editing // search // performance // clipboard // backup
