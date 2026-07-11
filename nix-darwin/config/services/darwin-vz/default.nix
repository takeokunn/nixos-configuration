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
  # Auto-shutdown after 3h idle. The idle monitor now fails safe — any
  # ESTABLISHED TCP connection to the guest (ssh-ng distributed builds,
  # deploy-rs) counts as activity, and an unobservable probe is treated as
  # busy rather than idle — so a long build is no longer misjudged as idle and
  # killed mid-deploy (fixed upstream in darwin-vz-nix 85e2583).
  services.darwin-vz.idleTimeout = 180;
  services.darwin-vz.kernelPath = "${darwin-vz-pkgs.guest-kernel}/Image";
  services.darwin-vz.initrdPath = "${darwin-vz-pkgs.guest-initrd}/initrd";
  services.darwin-vz.systemPath = "${darwin-vz-pkgs.guest-system}";
}
