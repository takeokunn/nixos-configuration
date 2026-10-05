{ pkgs, ... }:
{
  imports = [
    ./pandoc
    ./doggo
    ./lnav
  ];

  # Duplicate-code detection and the solvers and model checkers named by the formal-methods skill.
  home.packages = with pkgs; [
    similarity
    z3
    alloy6
    quint
    tlaplus
  ];
}
