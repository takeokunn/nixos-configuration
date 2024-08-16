{ username }: {
  rtkit.enable = true;
  tpm2.enable = true;
  audit.enable = true;
  sudo.extraConfig = ''
    ${username} ALL=NOPASSWD: ALL
  '';
}
