{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    mold
  ];
}
