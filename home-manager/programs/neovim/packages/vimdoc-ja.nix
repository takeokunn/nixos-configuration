{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPlugin {
  pname = "vimdoc-ja";
  version = "2024-09-04";
  src = fetchFromGitHub {
    owner = "vim-jp";
    repo = "vimdoc-ja";
    rev = "1fa7e5b4f332aec82d70db4524f80be3be45e992";
    hash = "sha256-aUZp/FjZSUeHa6L1cJdOC4pzvrNV3AiqXumLVZLEoig=";
  };
  meta.homepage = "https://github.com/vim-jp/vimdoc-ja";
}
