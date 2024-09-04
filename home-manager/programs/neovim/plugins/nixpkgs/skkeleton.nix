{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPlugin {
  pname = "skkeleton";
  version = "2024-09-04";
  src = fetchFromGitHub {
    owner = "vim-skk";
    repo = "skkeleton";
    rev = "882ec0c10856aadd0c2bf843b9b04cf16272a19b";
    hash = "sha256-b3tDU4iz0tV3mebSPlmvXDSrvw8hdR5w0EPyDXjaiqA=";
  };
  meta.homepage = "https://github.com/vim-skk/skkeleton";
}
