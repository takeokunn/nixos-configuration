{ pkgs, username }:
{
  services = {
    ntp.enable = true;
    cachix-agent.enable = true;
    pcscd.enable = true;

    ollama = {
      enable = true;
      package = pkgs.ollama-rocm;
    };
    offlineimap.enable = true;
    openssh.enable = true;

    xserver = {
      enable = true;
      xkb.layout = "jp";
    };

    displayManager = {
      gdm.enable = true;
      autoLogin = {
        enable = true;
        user = username;
      };
    };

    pulseaudio.enable = false;

    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
      jack.enable = true;

      extraConfig.pipewire."92-low-latency" = {
        "context.properties" = {
          "default.clock.rate" = 48000;
          "default.clock.quantum" = 128;
          "default.clock.min-quantum" = 32;
          "default.clock.max-quantum" = 1024;
        };
      };

      wireplumber.extraConfig."10-bluez" = {
        "monitor.bluez.properties" = {
          "bluez5.enable-sbc-xq" = true;
          "bluez5.enable-msbc" = true;
          "bluez5.enable-hw-volume" = true;
          "bluez5.roles" = [ "hsp_hs" "hsp_ag" "hfp_hf" "hfp_ag" "a2dp_sink" "a2dp_source" ];
        };
      };
    };

    xremap = {
      enable = true;
      userName = username;
      serviceMode = "system";
      config = {
        modmap = [
          {
            name = "CapsLock to Ctrl";
            remap = {
              CapsLock = "Ctrl_L";
            };
          }
        ];
      };
    };

    auto-epp = {
      enable = true;
      settings.Settings = {
        epp_state_for_AC = "balance_performance";
        epp_state_for_BAT = "power";
      };
    };
  };
}
