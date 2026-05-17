{ username }:
{
  security.rtkit.enable = true;
  security.tpm2.enable = true;
  security.audit.enable = true;
  security.sudo.extraConfig = ''
    ${username} ALL=NOPASSWD: ALL
  '';
}
