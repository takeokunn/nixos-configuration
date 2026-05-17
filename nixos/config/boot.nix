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
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.graceful = true;
  boot.loader.systemd-boot.consoleMode = "max";
  boot.loader.systemd-boot.configurationLimit = 10;
  boot.loader.efi.canTouchEfiVariables = true;
}
