{ pkgs, ... }:
{
  # Rootless and daemonless; `docker` is an alias for podman.
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    defaultNetwork.settings.dns_enabled = true;
  };

  # A per-user Docker API socket for tools that talk to the daemon directly, such as docker-compose.
  systemd.user.sockets.podman.wantedBy = [ "sockets.target" ];
  # Shell init only: pam_env runs before pam_systemd sets XDG_RUNTIME_DIR, so a PAM copy of this
  # variable cannot expand and only logs "Expandable variables must be wrapped in {}".
  environment.variables.DOCKER_HOST = "unix://$XDG_RUNTIME_DIR/podman/podman.sock";
  environment.systemPackages = [ pkgs.docker-compose ];
}
