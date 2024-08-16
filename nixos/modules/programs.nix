{ pkgs }: {
  fish.enable = true;
  firefox.enable = true;

  gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  sway = {
    enable = true;
    xwayland.enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [ swaylock swayidle foot rofi ];
  };

  light.enable = true;
  waybar.enable = true;
}
