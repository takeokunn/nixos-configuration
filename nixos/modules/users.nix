{ pkgs, username }: {
  users.${username} = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "video" "audio" "docker" ];
    shell = pkgs.fish;
    useDefaultShell = true;
    packages = with pkgs; [ alsa-utils wl-clipboard grim ];
  };
}
