{ pkgs }:
{
  home.packages = pkgs.lib.optionals pkgs.stdenv.isDarwin (
    with pkgs;
    [
      llvmPackages.clang
      llvmPackages.lld
    ]
  );

  programs.cargo.enable = true;
  programs.cargo.settings.build.jobs = 12;
  programs.cargo.settings.profile.dev.codegen-units = 16;
  # macOS Apple Silicon: use lld for faster linking
  programs.cargo.settings.target.aarch64-apple-darwin.linker = "clang";
  programs.cargo.settings.target.aarch64-apple-darwin.rustflags = [
    "-C"
    "link-arg=-fuse-ld=lld"
  ];
}
