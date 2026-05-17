{
  services.dnsmasq.enable = true;
  services.dnsmasq.addresses."*.localhost" = "127.0.0.1";
  services.dnsmasq.bind = "127.0.0.1";
}
