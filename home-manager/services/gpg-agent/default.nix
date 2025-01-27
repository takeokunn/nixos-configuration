{
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 60 * 60 * 24;
    defaultCacheTtlSsh = 60 * 60 * 24;
    maxCacheTtl = 60 * 60 * 24;
    maxCacheTtlSsh = 60 * 60 * 24;
    enableSshSupport = true;
    enableExtraSocket = true;
  };

  programs.fish = {
    interactiveShellInit = ''
      set -x SSH_AUTH_SOCK $(gpgconf --list-dirs agent-ssh-socket)
    '';
  };
}
