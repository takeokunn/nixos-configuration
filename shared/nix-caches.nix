# Binary caches every host trusts; each platform module adds its own on top.
{
  substituters = [
    "https://nix-community.cachix.org"
    "https://devenv.cachix.org"
    "https://takeokunn-nixos-configuration.cachix.org"
    "https://cache.numtide.com"
    "https://cclens.cachix.org"
  ];
  trusted-public-keys = [
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
    "takeokunn-nixos-configuration.cachix.org-1:TzNrelquzPLqhPJnUFUp/RC/eHrtEegOjVvGEw+jsdc="
    "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    "cclens.cachix.org-1:0QUNU6PuVyf+yXOvg3n1rd3FksBoB3s3/Jty50iKRNQ="
  ];
}
