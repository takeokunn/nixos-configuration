{
  programs.dconf.enable = true;

  services = {
    pulseaudio.enable = false;

    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
      jack.enable = true;

      # extraConfig.pipewire."92-high-quality" = {
      #   "context.properties" = {
      #     "default.clock.rate" = 48000;
      #     "default.clock.allowed-rates" = [
      #       44100
      #       48000
      #       88200
      #       96000
      #       176400
      #       192000
      #     ];
      #     "default.clock.quantum" = 128;
      #     "default.clock.min-quantum" = 32;
      #     "default.clock.max-quantum" = 1024;
      #   };
      #   "stream.properties" = {
      #     "resample.quality" = 10;
      #   };
      # };
      #
      # wireplumber.extraConfig."10-bluez" = {
      #   "monitor.bluez.properties" = {
      #     "bluez5.enable-sbc-xq" = true;
      #     "bluez5.enable-msbc" = true;
      #     "bluez5.enable-hw-volume" = true;
      #     "bluez5.codecs" = [
      #       "sbc"
      #       "sbc_xq"
      #       "aac"
      #       "ldac"
      #       "aptx"
      #       "aptx_hd"
      #     ];
      #     "bluez5.a2dp.ldac.quality" = "hq";
      #     "bluez5.roles" = [
      #       "hsp_hs"
      #       "hsp_ag"
      #       "hfp_hf"
      #       "hfp_ag"
      #       "a2dp_sink"
      #       "a2dp_source"
      #     ];
      #   };
      # };
      #
      # wireplumber.extraConfig."50-alsa-high-quality" = {
      #   "monitor.alsa.rules" = [
      #     {
      #       matches = [
      #         { "node.name" = "~alsa_output.*"; }
      #       ];
      #       actions.update-props = {
      #         "audio.format" = "S24LE";
      #         "audio.rate" = 48000;
      #         "api.alsa.period-size" = 256;
      #         "api.alsa.headroom" = 1024;
      #         "dither.method" = "wannamaker3";
      #         "dither.noise" = 2;
      #         "session.suspend-timeout-seconds" = 0;
      #       };
      #     }
      #     {
      #       matches = [
      #         { "node.name" = "~alsa_input.*"; }
      #       ];
      #       actions.update-props = {
      #         "audio.format" = "S24LE";
      #         "audio.rate" = 48000;
      #         "session.suspend-timeout-seconds" = 0;
      #       };
      #     }
      #   ];
      # };
    };
  };
}
