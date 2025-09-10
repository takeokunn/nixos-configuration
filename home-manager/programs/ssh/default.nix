{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    includes = [ "config.d/*" ];
    matchBlocks."*" = {
      compression = true;
      serverAliveInterval = 15;
    };
  };
}
