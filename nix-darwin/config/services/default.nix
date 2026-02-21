{ pkgs, emacsLib }:
let
  aerospace = import ./aerospace { inherit pkgs emacsLib; };
  darwin-vz = import ./darwin-vz;
  dnsmasq = import ./dnsmasq;
  nextdns = import ./nextdns;
in
{
  imports = [
    aerospace
    darwin-vz
    dnsmasq
    nextdns
  ];
}
