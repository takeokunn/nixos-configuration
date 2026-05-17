{
  networking.hostName = "nixos";
  networking.networkmanager.enable = true;
  networking.nat.enable = true;
  networking.nat.enableIPv6 = true;
  networking.nat.internalInterfaces = [ "ve-+" ];
  networking.nat.externalInterface = "ens3";
}
