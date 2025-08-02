{ pkgs, username }:
{
  users.users.${username} = {
    isNormalUser = true;
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
      wl-clipboard
      grim
    ];
  };
}
