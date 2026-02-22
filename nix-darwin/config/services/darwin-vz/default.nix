{ inputs, ... }:
let
  guestSystem = "aarch64-linux";
  darwin-vz-pkgs = inputs.darwin-vz-nix.packages.${guestSystem};
in
{
  services.darwin-vz = {
    enable = false;
    cores = 14;
    memory = 49152;
    diskSize = "200G";
    rosetta = true;
    maxJobs = 4;
    kernelPath = "${darwin-vz-pkgs.guest-kernel}/Image";
    initrdPath = "${darwin-vz-pkgs.guest-initrd}/initrd";
    systemPath = "${darwin-vz-pkgs.guest-system}";
  };
}
