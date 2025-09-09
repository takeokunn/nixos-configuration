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
    enable = false;
    arguments = [
      "-profile"
      "2c6735"
    ];
  };

  services.dnsmasq = {
    enable = true;
    addresses."*.localhost" = "127.0.0.1";
    bind = "127.0.0.1";
  };
}
