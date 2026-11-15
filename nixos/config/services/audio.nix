{
  programs.dconf.enable = true;

  services.pulseaudio.enable = false;

  services.pipewire.enable = true;
  services.pipewire.alsa.enable = true;
  services.pipewire.alsa.support32Bit = true;
  services.pipewire.pulse.enable = true;

  # Run at the source rate when it is 44.1 or 48 kHz instead of resampling everything to 48 kHz.
  services.pipewire.extraConfig.pipewire."10-clock-rates" = {
    "context.properties"."default.clock.allowed-rates" = [
      44100
      48000
    ];
  };
  services.pipewire.extraConfig.pipewire-pulse."10-resample-quality" = {
    "stream.properties"."resample.quality" = 10;
  };
  services.pipewire.extraConfig.client."10-resample-quality" = {
    "stream.properties"."resample.quality" = 10;
  };

  services.pipewire.wireplumber.extraConfig."10-bluez" = {
    "monitor.bluez.properties" = {
      "bluez5.enable-sbc-xq" = true;
      "bluez5.enable-msbc" = true;
      "bluez5.enable-hw-volume" = true;
    };
  };
}
