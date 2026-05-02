{ pkgs, lib }:
{
  services.easyeffects = lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
  };

  dconf.settings = lib.mkIf pkgs.stdenv.isLinux {
    "com/github/wwmm/easyeffects/streamoutputs" = {
      output-device = "alsa_output.pci-0000_06_00.6.HiFi__Headphones__sink";
      use-default-output-device = false;
    };
  };
}
