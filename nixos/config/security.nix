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

  # Memory-safe sudo; no NOPASSWD rules, so every escalation needs the YubiKey or the password.
  security.sudo.enable = false;
  security.sudo-rs = {
    enable = true;
    wheelNeedsPassword = true;
    execWheelOnly = true;
  };

  # Touching the YubiKey is enough for sudo, greetd and hyprlock; without it PAM falls back to the password.
  security.pam.u2f = {
    enable = true;
    control = "sufficient";
    settings = {
      cue = true;
      origin = "pam://${config.networking.hostName}";
      appid = "pam://${config.networking.hostName}";
      authfile = ../../hosts/X13Gen2/u2f-keys;
    };
  };
  # hyprlock comes from home-manager, which cannot declare a PAM service; without one it cannot unlock.
  security.pam.services.hyprlock = { };

  # Blocks USB devices plugged in later unless a rule allows them; internal ports stay usable.
  services.usbguard = {
    enable = true;
    IPCAllowedUsers = [ "root" ];
    rules = ''
      allow id 1d6b:*
      allow with-interface equals { 09:00:* }
      allow with-connect-type "hardwired"
      allow id 1050:*
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
