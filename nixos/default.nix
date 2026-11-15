{
  pkgs,
  inputs,
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

  display = import ./config/services/display.nix;
  compositor = import ./config/compositor.nix;

  network = import ./config/network.nix;
  wifi = import ./config/wifi.nix;

  secrets = import ./config/secrets.nix;
  security = import ./config/security.nix;

  zram = import ./config/zram.nix;
  sysctl = import ./config/sysctl.nix;

  nix = import ./config/nix.nix;

  shell = import ./config/shell.nix;

  users = import ./config/users.nix;
  fonts = import ./config/fonts.nix { inherit pkgs; };
  stylix = import ./config/stylix.nix;
  browser = import ./config/browser.nix;
  claudeCodeSettings = import ./config/claude-code-settings.nix {
    inherit pkgs username;
    guardAndGuide = inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default;
  };
  codexSettings = import ./config/codex-settings.nix { inherit pkgs inputs; };

  power = import ./config/services/power.nix;
  misc = import ./config/services/misc.nix;
  backup = import ./config/services/backup.nix;
  swayosd = import ./config/services/swayosd.nix { inherit pkgs; };

  virtualisation = import ./config/virtualisation.nix;
in
{
  system.stateVersion = "26.11";

  imports = [
    boot
    time
    i18n
    systemd
    hardware
    audio
    display
    compositor
    network
    wifi
    secrets
    security
    zram
    sysctl
    nix
    shell
    users
    fonts
    stylix
    browser
    claudeCodeSettings
    codexSettings
    power
    misc
    backup
    swayosd

    virtualisation
  ];
}
