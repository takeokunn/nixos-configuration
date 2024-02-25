{
  description = "takeokunn's nix system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
  };

  outputs = inputs@{ self, nixpkgs, nix-darwin }:
    {
      darwinConfigurations = (
        import ./systems/darwin {
          inherit self nix-darwin;
        }
      );
    };
}
