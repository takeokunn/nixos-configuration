{ username }:
{
  security.pam.services.sudo_local.enable = true;
  security.pam.services.sudo_local.touchIdAuth = true;
  security.pam.services.sudo_local.reattach = true;

  # NOPASSWD, scoped to exactly these two invocations, so sketchybar's
  # non-interactive click_script (no TTY, can't satisfy the Touch ID prompt
  # above) can toggle system-wide sleep without a broader sudo grant.
  security.sudo.extraConfig = ''
    ${username} ALL=(root) NOPASSWD: /usr/bin/pmset -a disablesleep 0, /usr/bin/pmset -a disablesleep 1
  '';

  # One-shot migration cleanup: the sketchybar sleep_prevent toggle used to spawn a
  # detached `pmset noidle` process (tracked by a PID file in home-manager/mac/sketchybar).
  # That mechanism is replaced by disablesleep above, so a process left running from
  # the old toggle would otherwise be orphaned -- untracked and unkillable from the UI.
  system.activationScripts.killStrayPmsetNoidle.text = ''
    /usr/bin/pkill -f 'pmset noidle' || true
  '';
}
