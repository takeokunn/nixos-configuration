{ pkgs, ... }:
{
  programs.chromium.enable = pkgs.stdenv.isLinux;
  programs.chromium.extensions = [
    { id = "gppongmhjkpfnbhagpmjfkannfbllamg"; }
    { id = "kfdibhbheajeacnkkakomaliggbgndcf"; }
    { id = "gighmmpiobklfepjocnamgkkbiglidom"; }
    { id = "hkgfoiooedgoejojocmhlaklaeopbecg"; }
    { id = "gfapcejdoghpoidkfodoiiffaaibpaem"; }
    { id = "cejijldbedfmdehondfmoadlkhgjcmkd"; }
  ];
}
