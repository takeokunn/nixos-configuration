{ pkgs }:
{
  services.ollama = {
    enable = false;
    package = pkgs.ollama;
    host = "0.0.0.0";
  };
}
