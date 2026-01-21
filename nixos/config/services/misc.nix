{ pkgs }:
{
  services = {
    ntp.enable = true;
    cachix-agent.enable = true;
    pcscd.enable = true;

    ollama = {
      enable = true;
      package = pkgs.ollama-rocm;
    };

    offlineimap.enable = true;
  };
}
