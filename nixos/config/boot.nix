{ lib, ... }:
{
  boot.kernelParams = [
    "quiet"
    "loglevel=3"
    "nowatchdog"
  ];
  boot.tmp.useTmpfs = true;
  boot.tmp.cleanOnBoot = true;
  boot.tmp.tmpfsSize = "50%";
  boot.loader.timeout = 3;
  boot.loader.efi.canTouchEfiVariables = true;

  # lanzaboote replaces systemd-boot's installer and signs every generation for Secure Boot.
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/var/lib/sbctl";
    configurationLimit = 10;
    # Keys are generated and enrolled on the first boot with the firmware in Setup Mode.
    autoGenerateKeys.enable = true;
    autoEnrollKeys = {
      enable = true;
      autoReboot = true;
    };
  };

  # The LUKS PIN prompt in initrd uses the same jp layout as the desktop.
  console.useXkbConfig = true;
  console.earlySetup = true;

  system.etc.overlay.enable = true;
  services.userborn.enable = true;
  # /etc is rebuilt on every boot; keeping passwd/group under the persisted /var/lib/nixos holds UIDs stable.
  services.userborn.passwordFilesLocation = "/var/lib/nixos";

  # Boot entry for recovering from a broken /etc overlay or a USBGuard rule that locks out a device.
  specialisation.rescue.configuration = {
    system.etc.overlay.enable = lib.mkForce false;
    services.usbguard.enable = lib.mkForce false;
  };
}
