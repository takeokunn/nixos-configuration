{ pkgs }:
{
  services.ollama = {
    enable = true;
    package = pkgs.ollama;
    host = "0.0.0.0";
  };
}
