{ config }:
{
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

    nat = {
      enable = true;
      enableIPv6 = true;
      internalInterfaces = [ "ve-+" ];
      externalInterface = "ens3";
    };
  };
}
