let
  aerospace = import ./aerospace;
  dnsmasq = import ./dnsmasq;
  nextdns = import ./nextdns;
  sketchybar = import ./sketchybar;
in
{
  imports = [
    aerospace
    dnsmasq
    nextdns
    sketchybar
  ];
}
