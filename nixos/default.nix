{
  pkgs,
  config,
  xremap,
  username,
  ...
}:
let
  # System
  boot = import ./config/boot.nix;
  time = import ./config/time.nix;
  i18n = import ./config/i18n.nix;
  systemd = import ./config/systemd.nix;

  # Hardware
  hardware = import ./config/hardware.nix;

  # Audio
  audio = import ./config/services/audio.nix;

  # Display
  display = import ./config/services/display.nix { inherit username; };
  compositor = import ./config/compositor.nix;

  # Input
  input = import ./config/services/input.nix { inherit pkgs username; };

  # Network
  network = import ./config/network.nix;
  wifi = import ./config/wifi.nix;

  # Security
  security = import ./config/security.nix { inherit username; };

  # Performance
  zram = import ./config/zram.nix;
  sysctl = import ./config/sysctl.nix;

  # Nix
  nix = import ./config/nix.nix;

  # Shell/Programs
  shell = import ./config/shell.nix;

  # Users
  users = import ./config/users.nix { inherit pkgs username; };
  fonts = import ./config/fonts.nix { inherit pkgs; };

  # Services
  power = import ./config/services/power.nix;
  misc = import ./config/services/misc.nix { inherit pkgs; };

  # Virtualization
  virtualisation = import ./config/virtualisation.nix;
  containers = import ./config/containers.nix;
in
{
  system.stateVersion = "24.11";

  imports = [
    xremap.nixosModules.default
    # System
    boot
    time
    i18n
    systemd
    # Hardware
    hardware
    # Audio
    audio
    # Display
    display
    compositor
    # Input
    input
    # Network
    network
    wifi
    # Security
    security
    # Performance
    zram
    sysctl
    # Nix
    nix
    # Shell/Programs
    shell
    # Users
    users
    fonts
    # Services
    power
    misc
    # Virtualization
    virtualisation
    containers
  ];
}
