{ pkgs, username }:
{
  users.users.${username} = {
    isNormalUser = true;
    hashedPassword = "$6$Fn6U6kyBKsUqKWS8$.bPQBq.4euz19YHLoHQSCWqxGB2eFSAd6TnotC33hmBZTbMgzqmx/4waEsu8M3yv7ODxs9VG2zc88qhQNTSWd1";
    extraGroups = [
      "networkmanager"
      "wheel"
      "video"
      "audio"
      "docker"
    ];
    shell = pkgs.fish;
    useDefaultShell = true;
    packages = with pkgs; [
      alsa-utils
      pavucontrol
      qpwgraph
      wl-clipboard
      grim
    ];
  };
}
