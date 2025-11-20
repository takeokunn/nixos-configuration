let
  display = import ./display.nix;
  editing = import ./editing.nix;
  search = import ./search.nix;
  performance = import ./performance.nix;
in
display // editing // search // performance
