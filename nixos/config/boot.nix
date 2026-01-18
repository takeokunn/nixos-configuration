{
  boot = {
    kernelParams = [
      "quiet"
      "loglevel=3"
      "nowatchdog"
    ];
    tmp = {
      useTmpfs = true;
      cleanOnBoot = true;
      tmpfsSize = "50%";
    };
    loader = {
      timeout = 3;
      systemd-boot = {
        enable = true;
        graceful = true;
        consoleMode = "max";
        configurationLimit = 10;
      };
      efi.canTouchEfiVariables = true;
    };
  };
}
