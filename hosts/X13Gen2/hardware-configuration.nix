{
  lib,
  modulesPath,
  ...
}:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  services.xserver.videoDrivers = [ "amdgpu" ];

  boot = {
    initrd = {
      availableKernelModules = [
        "nvme"
        "xhci_pci"
        "usb_storage"
        "sd_mod"
        "amdgpu"
      ];
      kernelModules = [ ];
      systemd.enable = true;
    };
    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  networking.useDHCP = lib.mkDefault true;
  hardware.enableRedistributableFirmware = true;
}
