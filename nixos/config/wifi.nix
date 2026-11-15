{ config, ... }:
{
  # SSID and PSK come from sops (secrets/x13gen2.yaml, key networkmanager-env).
  networking.networkmanager.ensureProfiles = {
    environmentFiles = [ config.sops.secrets.networkmanager-env.path ];
    profiles.home-wifi = {
      connection.id = "home-wifi";
      connection.type = "wifi";
      wifi.mode = "infrastructure";
      wifi.ssid = "$HOME_WIFI_SSID";
      wifi-security.auth-alg = "open";
      wifi-security.key-mgmt = "wpa-psk";
      wifi-security.psk = "$HOME_WIFI_PSK";
    };
  };
}
