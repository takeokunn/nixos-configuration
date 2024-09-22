{ pkgs }: {
  services.swayosd = {
    enable = pkgs.stdenv.isLinux;
    display = null;
  };
}
