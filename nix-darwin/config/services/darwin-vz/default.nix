{ inputs, ... }:
let
  guestSystem = "aarch64-linux";
  darwin-vz-pkgs = inputs.darwin-vz-nix.packages.${guestSystem};
in
{
  services.darwin-vz.enable = false;
  services.darwin-vz.cores = 14;
  services.darwin-vz.memory = 49152;
  services.darwin-vz.diskSize = "200G";
  services.darwin-vz.rosetta = true;
  services.darwin-vz.kernelPath = "${darwin-vz-pkgs.guest-kernel}/Image";
  services.darwin-vz.initrdPath = "${darwin-vz-pkgs.guest-initrd}/initrd";
  services.darwin-vz.systemPath = "${darwin-vz-pkgs.guest-system}";
}
