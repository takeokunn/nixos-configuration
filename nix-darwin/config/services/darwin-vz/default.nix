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
  # No idle auto-shutdown: the idle monitor does not count ssh-ng nix builds
  # (deploy-rs remoteBuild / distributed builds) as activity, so a long
  # substitute/build over ssh-ng gets misjudged as idle and the VM is killed
  # mid-deploy. Keep the builder always-on until that monitor is fixed upstream.
  services.darwin-vz.idleTimeout = 0;
  services.darwin-vz.kernelPath = "${darwin-vz-pkgs.guest-kernel}/Image";
  services.darwin-vz.initrdPath = "${darwin-vz-pkgs.guest-initrd}/initrd";
  services.darwin-vz.systemPath = "${darwin-vz-pkgs.guest-system}";
}
