{
  programs.gpg = {
    enable = true;
    mutableKeys = false;
    mutableTrust = false;
    publicKeys = [
      {
        source = ./pubkeys/0F79C0AB03FD7A1C.asc;
        trust = 5;
      }
    ];
  };
}
