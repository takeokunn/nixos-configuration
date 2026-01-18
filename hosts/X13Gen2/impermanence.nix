{ ... }:
{
  fileSystems."/persist".neededForBoot = true;
  fileSystems."/var/log".neededForBoot = true;

  boot.initrd.systemd.services.rollback = {
    description = "Rollback btrfs root subvolume to blank snapshot";
    wantedBy = [ "initrd.target" ];
    after = [ "dev-disk-by\\x2dlabel-nixos.device" ];
    before = [ "sysroot.mount" ];
    unitConfig.DefaultDependencies = "no";
    serviceConfig.Type = "oneshot";
    script = ''
      mkdir -p /mnt
      mount -t btrfs -o subvol=/ /dev/disk/by-label/nixos /mnt

      if [[ -e /mnt/@root ]]; then
        btrfs subvolume list -o /mnt/@root | cut -f9 -d' ' | while read subvolume; do
          btrfs subvolume delete "/mnt/$subvolume"
        done
        btrfs subvolume delete /mnt/@root
      fi

      btrfs subvolume create /mnt/@root
      umount /mnt
    '';
  };

  environment.persistence."/persist" = {
    hideMounts = true;
    directories = [
      "/etc/nixos"
      "/etc/ssh"
      "/etc/NetworkManager/system-connections"
      "/var/lib/nixos"
      "/var/lib/systemd/coredump"
      "/var/lib/bluetooth"
      "/var/lib/alsa"
    ];
    files = [
      "/etc/machine-id"
      "/etc/adjtime"
    ];
  };

  systemd.tmpfiles.rules = [
    "d /persist/home 0755 root root -"
    "d /persist/home/take 0700 take users -"
  ];

  programs.fuse.userAllowOther = true;

  security.sudo.extraConfig = ''
    Defaults lecture = never
  '';
}
