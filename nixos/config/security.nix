{
  config,
  pkgs,
  username,
  ...
}:
{
  security.rtkit.enable = true;
  security.tpm2.enable = true;
  security.audit.enable = true;
  # Without a reader the kernel audit queue overflows ("kauditd hold queue overflow").
  security.auditd.enable = true;

  # Memory-safe sudo. Wheel escalation needs the YubiKey or the password; the only NOPASSWD rules
  # are the btrbk module's, limited to btrfs, mkdir and readlink for the btrbk user.
  security.sudo.enable = false;
  security.sudo-rs = {
    enable = true;
    wheelNeedsPassword = true;
    # The btrbk user is not in wheel, so a wheel-only wrapper stops its snapshots with
    # "sudo: Permission denied"; sudoers still decides who may run what.
    execWheelOnly = false;
  };

  # Touching the YubiKey is enough for sudo, greetd and hyprlock; without it PAM falls back to the password.
  security.pam.u2f = {
    enable = true;
    control = "sufficient";
    settings = {
      cue = true;
      origin = "pam://${config.networking.hostName}";
      appid = "pam://${config.networking.hostName}";
      # Interpolated so the file is copied to the store and kept in the system closure; a bare
      # path renders as a location inside the flake source, which the target may not have.
      authfile = "${../../hosts/X13Gen2/u2f-keys}";
    };
  };
  # hyprlock comes from home-manager, which cannot declare a PAM service; without one it cannot unlock.
  security.pam.services.hyprlock = { };

  # Blocks USB devices plugged in later unless a rule allows them; internal ports stay usable.
  # The last two rules allow the internal Bluetooth and camera, which report connect-type
  # "not used" instead of "hardwired"; pinning them to their ports keeps external copies blocked.
  services.usbguard = {
    enable = true;
    IPCAllowedUsers = [ "root" ];
    rules = ''
      allow id 1d6b:*
      allow with-interface equals { 09:00:* }
      allow with-connect-type "hardwired"
      allow id 1050:*
      allow id 8087:0029 via-port "3-4"
      allow id 04f2:b70f via-port "5-2"
    '';
  };
  # The user may watch and list devices (for notifications); allowing one still needs sudo.
  systemd.tmpfiles.rules =
    let
      ipcAccess = pkgs.writeText "usbguard-ipc-${username}" ''
        Devices=list,listen
        Exceptions=listen
      '';
    in
    [
      "d /var/lib/usbguard/IPCAccessControl.d 0755 root root -"
      "L+ /var/lib/usbguard/IPCAccessControl.d/${username}: - - - - ${ipcAccess}"
    ];
  systemd.packages = [ pkgs.usbguard-notifier ];
  systemd.user.services.usbguard-notifier.wantedBy = [ "graphical-session.target" ];
}
