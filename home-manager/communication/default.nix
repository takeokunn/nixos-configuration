{ pkgs, ... }:
{
  home.packages = pkgs.lib.optionals (!pkgs.stdenv.isDarwin) (
    with pkgs;
    [
      discord
      slack
    ]
  );
}
