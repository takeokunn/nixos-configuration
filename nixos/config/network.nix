{ username, ... }:
{
  networking.hostName = "X13Gen2";

  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";
  # A random MAC address per network that stays stable across reconnects (iwd handles Wi-Fi under NetworkManager).
  networking.wireless.iwd.settings.General.AddressRandomization = "network";

  # Nothing at boot needs the network to be up, and waiting for Wi-Fi delays the greeter.
  systemd.services.NetworkManager-wait-online.enable = false;

  networking.nftables.enable = true;
  networking.firewall.enable = true;

  # Quad9 over DNS-over-TLS. For a captive portal, `resolvectl dnsovertls <link> no` lifts it temporarily.
  services.resolved = {
    enable = true;
    settings.Resolve = {
      DNS = [
        "9.9.9.9#dns.quad9.net"
        "149.112.112.112#dns.quad9.net"
        "2620:fe::fe#dns.quad9.net"
        "2620:fe::9#dns.quad9.net"
      ];
      DNSOverTLS = "true";
      Domains = [ "~." ];
      FallbackDNS = [ ];
    };
  };

  services.tailscale = {
    enable = true;
    openFirewall = true;
    # Lets the user run `tailscale serve` from the bar without sudo.
    extraSetFlags = [ "--operator=${username}" ];
  };

  # SSH is reachable only over the tailnet, by key.
  services.openssh = {
    enable = true;
    openFirewall = false;
    hostKeys = [
      {
        path = "/etc/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
    ];
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
      AllowUsers = [ username ];
    };
  };
  networking.firewall.interfaces.tailscale0.allowedTCPPorts = [ 22 ];
}
