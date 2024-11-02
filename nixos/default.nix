{
  pkgs,
  xremap,
  username,
  ...
}:
let
  boot = import ./config/boot.nix;
  fonts = import ./config/fonts.nix { inherit pkgs; };
  hardware = import ./config/hardware.nix;
  i18n = import ./config/i18n.nix;
  networking = import ./config/networking.nix;
  nix = import ./config/nix.nix;
  programs = import ./config/programs.nix;
  security = import ./config/security.nix { inherit username; };
  services = import ./config/services.nix { inherit username; };
  systemd = import ./config/systemd.nix;
  time = import ./config/time.nix;
  users = import ./config/users.nix { inherit pkgs username; };
  virtualisation = import ./config/virtualisation.nix;
in
{
  system.stateVersion = "24.05";
  imports = [
    xremap.nixosModules.default
    boot
    fonts
    hardware
    i18n
    networking
    nix
    programs
    security
    services
    systemd
    time
    users
    virtualisation
  ];
}
