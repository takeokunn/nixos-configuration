let
  display = import ./display.nix;
  editing = import ./editing.nix;
  search = import ./search.nix;
in
display // editing // search
