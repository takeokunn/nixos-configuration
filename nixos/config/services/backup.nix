{
  # Hourly read-only snapshots of /persist, the only data that survives a reboot.
  services.btrbk.instances.persist = {
    onCalendar = "hourly";
    settings = {
      timestamp_format = "long";
      snapshot_preserve_min = "2h";
      snapshot_preserve = "24h 7d 4w";
      volume."/.btrfs" = {
        snapshot_dir = "@snapshots";
        subvolume."@persist" = { };
      };
    };
  };
}
