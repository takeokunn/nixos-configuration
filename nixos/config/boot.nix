{
  boot = {
    hardwareScan = true;
    tmp = {
      useTmpfs = true;
      cleanOnBoot = true;
    };
    loader = {
      systemd-boot = {
        enable = true;
        graceful = true;
      };
      efi.canTouchEfiVariables = true;
    };
  };
}
