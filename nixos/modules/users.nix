{ pkgs, username }: {
  users.${username} = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "video" "audio" ];
    shell = pkgs.fish;
    useDefaultShell = true;
  };
}
