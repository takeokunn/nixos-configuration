{
  pkgs,
  xremap,
  username,
  ...
}:
let
  boot = import ./config/boot.nix;
  time = import ./config/time.nix;
  i18n = import ./config/i18n.nix;
  systemd = import ./config/systemd.nix;

  hardware = import ./config/hardware.nix;

  audio = import ./config/services/audio.nix;

  display = import ./config/services/display.nix { inherit username; };
  compositor = import ./config/compositor.nix;

  input = import ./config/services/input.nix { inherit username; };

  network = import ./config/network.nix;
  wifi = import ./config/wifi.nix;

  security = import ./config/security.nix { inherit username; };

  zram = import ./config/zram.nix;
  sysctl = import ./config/sysctl.nix;

  nix = import ./config/nix.nix;

  shell = import ./config/shell.nix;

  users = import ./config/users.nix { inherit pkgs username; };
  fonts = import ./config/fonts.nix { inherit pkgs; };

  power = import ./config/services/power.nix;
  misc = import ./config/services/misc.nix;
  swayosd = import ./config/services/swayosd.nix { inherit pkgs; };

  virtualisation = import ./config/virtualisation.nix;
  containers = import ./config/containers.nix;
in
{
  system.stateVersion = "24.11";

  imports = [
    xremap.nixosModules.default
    boot
    time
    i18n
    systemd
    hardware
    audio
    display
    compositor
    input
    network
    wifi
    security
    zram
    sysctl
    nix
    shell
    users
    fonts
    power
    misc
    swayosd

    virtualisation
    containers
  ];
}
