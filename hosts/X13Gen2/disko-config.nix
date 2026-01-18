{ ... }:
let
  defaultMountOptions = [
    "compress=zstd:1"
    "noatime"
  ];
in
{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = "/dev/nvme0n1";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "1G";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
            luks = {
              size = "100%";
              label = "cryptroot";
              content = {
                type = "luks";
                name = "cryptroot";
                extraOpenArgs = [
                  "--allow-discards"
                  "--perf-no_read_workqueue"
                  "--perf-no_write_workqueue"
                ];
                settings = {
                  allowDiscards = true;
                };
                passwordFile = "/tmp/luks-password";
                content = {
                  type = "btrfs";
                  extraArgs = [
                    "-f"
                    "-L"
                    "nixos"
                  ];
                  subvolumes = {
                    "@root" = {
                      mountpoint = "/";
                      mountOptions = defaultMountOptions;
                    };
                    "@home" = {
                      mountpoint = "/home";
                      mountOptions = defaultMountOptions;
                    };
                    "@nix" = {
                      mountpoint = "/nix";
                      mountOptions = defaultMountOptions;
                    };
                    "@persist" = {
                      mountpoint = "/persist";
                      mountOptions = defaultMountOptions;
                    };
                    "@log" = {
                      mountpoint = "/var/log";
                      mountOptions = defaultMountOptions;
                    };
                    "@swap" = {
                      mountpoint = "/.swapvol";
                      swap.swapfile.size = "8G";
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
}
