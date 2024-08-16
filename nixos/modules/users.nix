{ pkgs, username }: {
  users.${username} = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "pipewire" "video" ];
    shell = pkgs.fish;
    useDefaultShell = true;
  };
}
