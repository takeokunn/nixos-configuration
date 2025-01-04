{ emacsPkg, ... }:
{
  services.emacs = {
    enable = true;
    package = emacsPkg;
  };
}
