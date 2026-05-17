{
  programs.gpg.enable = true;
  programs.gpg.mutableKeys = false;
  programs.gpg.mutableTrust = false;
  programs.gpg.publicKeys = [
    {
      source = ./pubkeys/0F79C0AB03FD7A1C.asc;
      trust = 5;
    }
  ];
}
