{ emacsLib }:
let
  aerospace = import ./aerospace { inherit emacsLib; };
  darwin-vz = import ./darwin-vz;
  dnsmasq = import ./dnsmasq;
  nextdns = import ./nextdns;
  tailscale = import ./tailscale;
in
{
  imports = [
    aerospace
    darwin-vz
    dnsmasq
    nextdns
    tailscale
  ];
}
