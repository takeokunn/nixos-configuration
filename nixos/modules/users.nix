{ pkgs, username }: {
  users.${username} = {
    isNormalUser = true;
    extraGroups = [ "networkmanager" "wheel" "pipewire" ];
    shell = pkgs.fish;
    useDefaultShell = true;
  };
}
