{ username }: {
  sudo.extraConfig = ''
    ${username} ALL=NOPASSWD: ALL
  '';
  pam.enableSudoTouchIdAuth = true;
}
