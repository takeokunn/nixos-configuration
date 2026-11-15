{
  services.pcscd.enable = true;
  services.fwupd.enable = true;

  services.btrfs.autoScrub = {
    enable = true;
    fileSystems = [ "/persist" ];
  };

  services.journald.settings.Journal.SystemMaxUse = "1G";
}
