{ pkgs }:
{
  home.packages =
    pkgs.lib.optionals pkgs.stdenv.isDarwin (
      with pkgs;
      [
        llvmPackages.clang
        llvmPackages.lld
      ]
    );

  programs.cargo = {
    enable = true;
    settings = {
      build.jobs = 12;
      profile.dev.codegen-units = 16;

      # macOS Apple Silicon: use lld for faster linking
      target.aarch64-apple-darwin = {
        linker = "clang";
        rustflags = [
          "-C"
          "link-arg=-fuse-ld=lld"
        ];
      };
    };
  };
}
