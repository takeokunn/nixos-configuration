let
  band = type: frequency: gain: q: {
    inherit
      type
      frequency
      gain
      q
      ;
    mode = "RLC (BT)";
    slope = "x1";
    mute = false;
    solo = false;
  };
  bands =
    list:
    builtins.listToAttrs (
      map (i: {
        name = "band${toString i}";
        value = builtins.elemAt list i;
      }) (builtins.genList (i: i) (builtins.length list))
    );

  # Small laptop speakers: cut what they cannot reproduce, soften boxiness, lift voice clarity,
  # and let the bass enhancer add harmonics instead of boosting real low end.
  speakerBands = bands [
    (band "Hi-pass" 120.0 0.0 0.7)
    (band "Bell" 450.0 (-2.5) 1.2)
    (band "Bell" 3000.0 2.5 1.0)
    (band "Hi-shelf" 9000.0 2.0 0.7)
  ];

  limiter = {
    bypass = false;
    threshold = -1.0;
    attack = 5.0;
    release = 50.0;
  };
in
{
  services.easyeffects = {
    enable = true;
    preset = {
      output = "speakers";
      input = "voice";
    };
    extraPresets = {
      speakers.output = {
        blocklist = [ ];
        plugins_order = [
          "equalizer#0"
          "bass_enhancer#0"
          "limiter#0"
        ];
        "equalizer#0" = {
          bypass = false;
          mode = "IIR";
          num-bands = 4;
          split-channels = false;
          input-gain = -3.0;
          output-gain = 0.0;
          left = speakerBands;
          right = speakerBands;
        };
        "bass_enhancer#0" = {
          bypass = false;
          amount = 4.0;
          harmonics = 8.5;
          scope = 120.0;
          floor = 40.0;
          floor-active = true;
          blend = 0.0;
        };
        "limiter#0" = limiter;
      };
      # Headphones and Bluetooth reproduce the full range; only guard against clipping.
      headphones.output = {
        blocklist = [ ];
        plugins_order = [ "limiter#0" ];
        "limiter#0" = limiter;
      };
      voice.input = {
        blocklist = [ ];
        plugins_order = [
          "rnnoise#0"
          "autogain#0"
        ];
        "rnnoise#0" = {
          bypass = false;
          enable-vad = true;
          vad-thres = 50.0;
          wet = 0.0;
        };
        "autogain#0" = {
          bypass = false;
          target = -23.0;
        };
      };
    };
  };
}
