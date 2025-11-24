let
  aerospace = import ./aerospace;
  dnsmasq = import ./dnsmasq;
  nextdns = import ./nextdns;
in
{
  imports = [
    aerospace
    dnsmasq
    nextdns
  ];
}
