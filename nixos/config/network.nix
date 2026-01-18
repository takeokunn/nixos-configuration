{
  networking = {
    hostName = "nixos";
    networkmanager.enable = true;

    nat = {
      enable = true;
      enableIPv6 = true;
      internalInterfaces = [ "ve-+" ];
      externalInterface = "ens3";
    };
  };
}
