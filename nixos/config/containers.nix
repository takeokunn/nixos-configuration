{
  containers = {
    alice = {
      autoStart = true;

      privateNetwork = true;
      hostAddress = "192.168.100.10";
      localAddress = "192.168.100.11";
      hostAddress6 = "fc00::1";
      localAddress6 = "fc00::2";

      config =
        {
          pkgs,
          lib,
          ...
        }:
        {
          system.stateVersion = "25.05";

          environment.systemPackages = with pkgs; [ git ];

          users.users.take = {
            isNormalUser = true;
            home = "/home/take";
            extraGroups = [ "wheel" ];
          };

          services = {
            openssh.enable = true;
            httpd.enable = true;
            resolved.enable = true;
          };

          networking = {
            useHostResolvConf = lib.mkForce false;
            firewall = {
              enable = true;
              allowedTCPPorts = [
                22
                80
              ];
            };
          };
        };
    };
  };
}
