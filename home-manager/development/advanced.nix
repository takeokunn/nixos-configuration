{ pkgs, ... }:
let
  gnuTimeout = pkgs.runCommand "gnu-timeout" { } ''
    mkdir -p "$out/bin"
    ln -s "${pkgs.coreutils}/bin/timeout" "$out/bin/timeout"
  '';
in
{
  imports = [
    ./pandoc
    ./doggo
    ./lnav
    ./language-servers.nix
  ];

  # Duplicate-code detection.
  home.packages = with pkgs; [
    similarity
    ast-grep
    gnuTimeout
  ];
}
