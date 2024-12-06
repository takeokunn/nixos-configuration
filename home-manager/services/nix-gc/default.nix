{
  nix.gc = {
    automatic = true;
    frequency = "monthly";
    options = "--delete-older-than 3d";
  };
}
