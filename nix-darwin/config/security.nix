{ username }:
{
  security.pam.services.sudo_local.enable = true;
  security.pam.services.sudo_local.touchIdAuth = true;
  security.pam.services.sudo_local.reattach = true;

  # NOPASSWD, scoped to exactly these two invocations, so sketchybar's
  # non-interactive click_script (no TTY, can't satisfy the Touch ID prompt
  # above) can toggle system-wide sleep without a broader sudo grant.
  #
  # Same rationale for the tailscale serve pair: sketchybar's toggle needs to
  # start/stop serving Mediator (127.0.0.1:43100) over the tailnet without a
  # TTY. Scoped to exactly these two invocations, not a `tailscale serve *`
  # wildcard, so the grant can't be reused to serve an arbitrary port.
  security.sudo.extraConfig = ''
    ${username} ALL=(root) NOPASSWD: /usr/bin/pmset -a disablesleep 0, /usr/bin/pmset -a disablesleep 1, /run/current-system/sw/bin/tailscale serve --bg 43100, /run/current-system/sw/bin/tailscale serve --https=443 off
  '';

  # One-shot migration cleanup: the sketchybar sleep_prevent toggle used to spawn a
  # detached `pmset noidle` process (tracked by a PID file in home-manager/mac/sketchybar).
  # That mechanism is replaced by disablesleep above, so a process left running from
  # the old toggle would otherwise be orphaned -- untracked and unkillable from the UI.
  system.activationScripts.killStrayPmsetNoidle.text = ''
    /usr/bin/pkill -f 'pmset noidle' || true
  '';
}
