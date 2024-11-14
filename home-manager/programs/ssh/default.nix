{
  programs.ssh = {
    enable = true;
    compression = true;
    serverAliveInterval = 15;
    includes = [ "config.d/*" ];
  };
}
