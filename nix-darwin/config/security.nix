{ username }:
{
  security = {
    sudo.extraConfig = ''
      ${username} ALL=NOPASSWD: ALL
    '';
    pam.services.sudo_local = {
      touchIdAuth = true;
      reattach = true;
    };
  };
}
