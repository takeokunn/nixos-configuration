{ username, ... }:
{
  security.pam.services.sudo_local.enable = true;
  security.pam.services.sudo_local.touchIdAuth = true;
  security.pam.services.sudo_local.reattach = true;

  # NOPASSWD is scoped to the two tailscale serve commands below so sketchybar's
  # toggle can start/stop serving Mediator (127.0.0.1:43100) over the tailnet
  # without a TTY. Exact commands avoid a `tailscale serve *` wildcard that could
  # be reused to serve an arbitrary port.
  security.sudo.extraConfig = ''
    ${username} ALL=(root) NOPASSWD: /run/current-system/sw/bin/tailscale serve --bg 43100, /run/current-system/sw/bin/tailscale serve --https=443 off
  '';
}
