{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPlugin {
  pname = "denops-helloworld";
  version = "2024-09-04";
  src = fetchFromGitHub {
    owner = "vim-denops";
    repo = "denops-helloworld.vim";
    rev = "f975281571191cfd4e3f9e5ba77103932f7dd6e5";
    hash = "sha256-w+UIj+IM/Me4Jp81bujlQLpOGUssWEqmZ3xUmWn5+mE=";
  };
  meta.homepage = "https://github.com/vim-denops/denops-helloworld.vim";
}
