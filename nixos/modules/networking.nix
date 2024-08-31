{ config }: {
  sops = {
    defaultSopsFile = ../../secrets/network.yaml;
    age.sshKeyPaths = [ "/home/take/.ssh/id_ed25519" ];
    secrets."home-wifi" = { };
  };

  networking = {
    hostName = "nixos";
    networkmanager = {
      enable = true;
      ensureProfiles.profiles = {
        home-wifi = {
          connection = {
            id = "Buffalo-G-90E0";
            type = "wifi";
          };
          wifi = {
            mode = "infrastructure";
            ssid = "Buffalo-G-90E0";
          };
          wifi-security = {
            auth-alg = "open";
            key-mgmt = "wpa-psk";
            psk = "uengspy3bwdkb";
          };
        };
      };
    };
    enableIPv6 = true;
    firewall.enable = true;
    nameservers = [
      "8.8.8.8"
      "8.8.4.4"
      "1.1.1.1"
      "1.0.0.1"
      "2001:4860:4860::8888"
      "2001:4860:4860::8844"
      "2606:4700:4700::1111"
      "2606:4700:4700::1001"
    ];
  };
}
