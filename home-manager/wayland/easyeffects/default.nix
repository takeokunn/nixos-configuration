{
  services.easyeffects.enable = true;

  dconf.settings."com/github/wwmm/easyeffects/streamoutputs".output-device =
    "alsa_output.pci-0000_06_00.6.HiFi__Headphones__sink";
  dconf.settings."com/github/wwmm/easyeffects/streamoutputs".use-default-output-device = false;
}
