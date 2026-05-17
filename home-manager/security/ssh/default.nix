{
  programs.ssh.enable = true;
  programs.ssh.enableDefaultConfig = false;
  programs.ssh.includes = [ "config.d/*" ];
  programs.ssh.matchBlocks."*".compression = true;
  programs.ssh.matchBlocks."*".serverAliveInterval = 15;
}
