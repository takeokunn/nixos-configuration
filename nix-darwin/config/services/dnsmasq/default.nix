{
  services.dnsmasq = {
    enable = true;
    addresses."*.localhost" = "127.0.0.1";
    bind = "127.0.0.1";
  };
}
