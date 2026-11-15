{
  # Raycast-style launcher and clipboard history (Mod+D, Mod+V in niri).
  programs.vicinae = {
    enable = true;
    systemd = {
      enable = true;
      autoStart = true;
    };
  };
  stylix.targets.vicinae.enable = true;
}
