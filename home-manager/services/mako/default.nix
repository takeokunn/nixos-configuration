{ pkgs }: {
  services.mako = {
    enable = pkgs.stdenv.isLinux;
    backgroundColor = "#282a36";
    textColor = "#44475a";
    borderColor = "#282a36";
    extraConfig = ''
      [urgency=low]
      border-color=#282a36

      [urgency=normal]
      border-color=#f1fa8c

      [urgency=high]
      border-color=#ff5555
    '';
  };
}
