{ pkgs, username }:
{
  users.users.${username}.isNormalUser = true;
  users.users.${username}.hashedPassword =
    "$6$Fn6U6kyBKsUqKWS8$.bPQBq.4euz19YHLoHQSCWqxGB2eFSAd6TnotC33hmBZTbMgzqmx/4waEsu8M3yv7ODxs9VG2zc88qhQNTSWd1";
  users.users.${username}.extraGroups = [
    "networkmanager"
    "wheel"
    "video"
    "audio"
    "docker"
  ];
  users.users.${username}.shell = pkgs.fish;
  users.users.${username}.packages = with pkgs; [
    alsa-utils
    pavucontrol
    qpwgraph
    wl-clipboard
    grim
  ];
}
