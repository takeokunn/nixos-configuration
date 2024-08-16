{ pkgs, username }: {
  users.${username} = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "pipewire" "video" "audio" ];
    shell = pkgs.fish;
    useDefaultShell = true;
  };
}
