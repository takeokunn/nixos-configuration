{
  programs.ssh.enable = true;
  programs.ssh.enableDefaultConfig = false;
  programs.ssh.includes = [ "config.d/*" ];
  programs.ssh.settings."*".compression = true;
  programs.ssh.settings."*".serverAliveInterval = 15;
  programs.ssh.settings."*".serverAliveCountMax = 3;
  programs.ssh.settings."*".connectTimeout = 10;
  programs.ssh.settings."*".controlMaster = "auto";
  programs.ssh.settings."*".controlPath = "~/.ssh/control/%C";
  programs.ssh.settings."*".controlPersist = "10m";

  home.activation.createSshControlDir = {
    after = [ "writeBoundary" ];
    before = [ ];
    data = ''
      install -d -m 0700 "$HOME/.ssh/control"
    '';
  };
}
