{ pkgs }:
{
  services.ollama = {
    enable = true;
    host = "0.0.0.0";
  };
}
