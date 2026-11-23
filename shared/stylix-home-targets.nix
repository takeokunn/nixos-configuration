# home-manager targets themed by Stylix on every host that runs it (NixOS and nix-darwin).
# fish, fzf, tmux and nixvim take Dracula's color roles from the Stylix palette instead
# (shared/dracula-roles.nix): Stylix's fish target rewrites the terminal palette on every shell start.
{
  stylix.targets.kitty.enable = true;
  stylix.targets.bat.enable = true;
  stylix.targets.k9s.enable = true;
}
