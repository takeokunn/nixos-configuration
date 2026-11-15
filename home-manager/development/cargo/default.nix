{ pkgs, ... }:
{
  home.packages =
    pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin (
      with pkgs;
      [
        llvmPackages.clang
        llvmPackages.lld
      ]
    )
    ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.mold ];

  programs.cargo.enable = true;
  programs.cargo.settings.build.jobs = 12;
  programs.cargo.settings.profile.dev.codegen-units = 16;
  programs.cargo.settings.target.aarch64-apple-darwin.linker = "clang";
  programs.cargo.settings.target.aarch64-apple-darwin.rustflags = [
    "-C"
    "link-arg=-fuse-ld=lld"
  ];
  # The linker driver stays whatever `cc` the environment provides (a devShell's own stdenv);
  # only the backend becomes mold, replacing rustup's rust-lld default and nixpkgs rustc's GNU ld.
  programs.cargo.settings.target.x86_64-unknown-linux-gnu.rustflags = [
    "-C"
    "linker-features=-lld"
    "-C"
    "link-arg=-fuse-ld=mold"
  ];
}
