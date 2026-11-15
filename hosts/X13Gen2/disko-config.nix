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
              # lanzaboote keeps a signed kernel and initrd per generation on the ESP.
              size = "2G";
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
                # Without --sector-size, cryptsetup picks 512 on NVMe drives that report 512-byte sectors.
                extraFormatArgs = [
                  "--type"
                  "luks2"
                  "--sector-size"
                  "4096"
                ];
                extraOpenArgs = [
                  "--allow-discards"
                  "--perf-no_read_workqueue"
                  "--perf-no_write_workqueue"
                ];
                settings = {
                  allowDiscards = true;
                  bypassWorkqueues = true;
                  crypttabExtraOpts = [ "tpm2-device=auto" ];
                };
                passwordFile = "/tmp/luks-password";
                content = {
                  type = "btrfs";
                  extraArgs = [
                    "-f"
                    "-L"
                    "nixos"
                    "--csum"
                    "xxhash"
                  ];
                  subvolumes = {
                    # Holds /home as well; both are recreated on every boot.
                    "@root" = {
                      mountpoint = "/";
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
                    # A separate subvolume keeps container images out of the @persist snapshots.
                    "@containers" = {
                      mountpoint = "/persist/home/take/.local/share/containers";
                      mountOptions = defaultMountOptions;
                    };
                    "@log" = {
                      mountpoint = "/var/log";
                      mountOptions = defaultMountOptions;
                    };
                    "@snapshots" = { };
                    "@swap" = {
                      mountpoint = "/.swapvol";
                      # Sized to RAM so a hibernation image always fits.
                      swap.swapfile.size = "16G";
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

  # The top-level subvolume, where btrbk finds @persist and @snapshots.
  fileSystems."/.btrfs" = {
    device = "/dev/mapper/cryptroot";
    fsType = "btrfs";
    options = [
      "subvol=/"
      "noatime"
    ];
  };
}
