{
  nix.gc = {
    automatic = true;
    dates = "monthly";
    options = "--delete-older-than 3d";
  };
}
