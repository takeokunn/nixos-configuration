let
  basic = import ./basic.nix;
  window = import ./window.nix;
in
basic ++ window
