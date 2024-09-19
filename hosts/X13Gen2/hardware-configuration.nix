{ config, lib, modulesPath, ... }: {
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  services.xserver.videoDrivers = [ "amdgpu" ];

  boot = {
    initrd = {
      availableKernelModules =
        [ "nvme" "xhci_pci" "usb_storage" "sd_mod" "amdgpu" ];
      kernelModules = [ ];
    };
    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/643f028c-88a5-45af-b7b8-52d78f05a501";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/A44D-7A24";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  networking.useDHCP = lib.mkDefault true;
  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}
