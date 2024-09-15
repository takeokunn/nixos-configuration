{
  boot = {
    hardwareScan = true;
    tmp = {
      useTmpfs = true;
      cleanOnBoot = true;
      tmpfsSize = "95%";
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
