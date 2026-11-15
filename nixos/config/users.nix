{
  config,
  pkgs,
  username,
  ...
}:
{
  users.mutableUsers = false;
  users.users.${username} = {
    isNormalUser = true;
    # Fixed so files under /persist keep their owner across reinstalls.
    uid = 1000;
    hashedPasswordFile = config.sops.secrets.take-hashed-password.path;
    extraGroups = [
      "networkmanager"
      "wheel"
      "video"
      "audio"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEZOYVVWqSDNiLasJKggu87+qbirAagxzHZeNQbhG4xs"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKicPueFUb/l/6u0QeuMRtj1qnFY07HNAQb9gyO6cZQl"
    ];
    shell = pkgs.fish;
    packages = with pkgs; [
      alsa-utils
      pavucontrol
      qpwgraph
      wl-clipboard
      grim
    ];
  };
}
