{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPlugin {
  pname = "ddu";
  version = "2024-09-04";
  src = fetchFromGitHub {
    owner = "Shougo";
    repo = "ddu.vim";
    rev = "b65b10dcd986457b5fa2f007820839d01ab7a215";
    hash = "sha256-FKjvghRqJSXRmaIifIyfhOAy1Q/dVy4UkErME43mITI=";
  };
}
