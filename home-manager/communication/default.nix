{ pkgs, isDarwin ? false }:
[
  {
    home.packages = pkgs.lib.optionals (!isDarwin) (
      with pkgs;
      [
        discord
        slack
      ]
    );
  }
]
