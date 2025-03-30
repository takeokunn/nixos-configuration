let
  aerospace = import ./aerospace;
  sketchybar = import ./sketchybar;
in
{
  imports = [
    aerospace
    sketchybar
  ];

  services.nextdns = {
    enable = true;
    arguments = [
      "-profile"
      "2c6735"
    ];
  };
}
