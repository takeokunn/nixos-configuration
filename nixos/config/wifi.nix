{
  networking.networkmanager.ensureProfiles.profiles = {
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
}
