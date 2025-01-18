{ pkgs }:
{
  services.ollama = {
    enable = pkgs.stdenv.isLinux;
  };
}
