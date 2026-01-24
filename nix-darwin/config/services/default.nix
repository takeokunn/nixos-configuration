{ pkgs, emacsLib }:
let
  aerospace = import ./aerospace { inherit pkgs emacsLib; };
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
