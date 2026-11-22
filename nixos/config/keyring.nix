{ lib, pkgs, ... }:
{
  # oo7 replaces gnome-keyring as the Secret Service. It unlocks the keyring from the systemd
  # credential oo7.keyring-encryption-password (docs/X13Gen2.org), so a YubiKey-only login,
  # which gives PAM no password, still opens it.
  services.oo7.enable = true;
  # services.oo7 adds pam_oo7 to login, which greetd includes. The daemon tries that PAM
  # password before the credential, so a login password that differs from the keyring
  # password would leave the keyring locked.
  security.pam.services.login.oo7.enable = lib.mkForce false;

  # programs.niri enables gnome-keyring and routes the Secret portal to it.
  services.gnome.gnome-keyring.enable = false;
  xdg.portal.config.niri."org.freedesktop.impl.portal.Secret" = lib.mkForce "oo7-portal";

  # oo7 asks org.gnome.keyring.SystemPrompter for passwords; of the gcr packages only gcr 3
  # still ships that prompter.
  services.dbus.packages = [ pkgs.gcr_3 ];

  # oo7-daemon mlockall()s itself so secrets stay out of swap. Its upstream unit sets
  # PrivateUsers=yes, so the wrapper's CAP_IPC_LOCK only holds inside that user namespace and
  # RLIMIT_MEMLOCK (8M) applies; mlockall then fails. A user unit cannot exceed the user
  # manager's hard limit, so user@ raises only the hard limit and oo7-daemon alone uses it.
  systemd.services."user@".serviceConfig.LimitMEMLOCK = "8M:infinity";
  systemd.user.services.oo7-daemon.serviceConfig.LimitMEMLOCK = "infinity";
}
