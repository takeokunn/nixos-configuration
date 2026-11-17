{ inputs, pkgs, ... }:
{
  system.stateVersion = "26.11";

  _module.args.guardAndGuide =
    inputs.guard-and-guide.packages.${pkgs.stdenv.hostPlatform.system}.default;

  imports = [
    ./config/boot.nix
    ./config/time.nix
    ./config/i18n.nix
    ./config/systemd.nix
    ./config/hardware.nix
    ./config/services/audio.nix
    ./config/services/display.nix
    ./config/compositor.nix
    ./config/network.nix
    ./config/wifi.nix
    ./config/secrets.nix
    ./config/security.nix
    ./config/zram.nix
    ./config/sysctl.nix
    ./config/nix.nix
    ./config/shell.nix
    ./config/users.nix
    ./config/fonts.nix
    ./config/stylix.nix
    ./config/browser.nix
    ./config/claude-code-settings.nix
    ./config/codex-settings.nix
    ./config/services/power.nix
    ./config/services/misc.nix
    ./config/services/backup.nix
    ./config/services/swayosd.nix
    ./config/virtualisation.nix
  ];
}
