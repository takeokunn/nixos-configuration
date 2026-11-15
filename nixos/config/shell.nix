{
  programs.fish.enable = true;
  programs.nix-ld.enable = true;

  # nix-index-database ships a prebuilt index, so the command-not-found handler works without channels.
  programs.nix-index-database.comma.enable = true;
}
