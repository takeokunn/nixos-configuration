{ lib, ... }:
{
  fileSystems."/persist".neededForBoot = true;
  fileSystems."/var/log".neededForBoot = true;

  boot.initrd.systemd.services.rollback = {
    description = "Rollback btrfs root subvolume to blank snapshot";
    wantedBy = [ "initrd.target" ];
    after = [ "cryptsetup.target" ];
    before = [ "sysroot.mount" ];
    unitConfig.DefaultDependencies = "no";
    serviceConfig.Type = "oneshot";
    script = ''
      mkdir -p /mnt
      mount -t btrfs -o subvol=/ /dev/mapper/cryptroot /mnt

      if [[ -e /mnt/@root ]]; then
        mkdir -p /mnt/@old_roots
        timestamp=$(date +%Y%m%d_%H%M%S)
        mv /mnt/@root "/mnt/@old_roots/@root_$timestamp"
      fi

      # Delete old roots older than 7 days
      for old in /mnt/@old_roots/@root_*; do
        if [[ -e "$old" ]]; then
          create_time=$(stat -c %Y "$old")
          current_time=$(date +%s)
          age_days=$(( (current_time - create_time) / 86400 ))
          if [[ $age_days -gt 7 ]]; then
            btrfs subvolume list -o "$old" | cut -f9 -d' ' | while read subvolume; do
              btrfs subvolume delete "/mnt/$subvolume" 2>/dev/null || true
            done
            btrfs subvolume delete "$old" 2>/dev/null || true
          fi
        fi
      done

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
      "/var/lib/docker"
      "/var/lib/libvirt"
      "/var/lib/tailscale"
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
