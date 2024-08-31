{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "ob-phpstan";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "emacs-php";
    repo = "ob-phpstan";
    rev = "c77355e70703affffb166b7100b9e9f3efb21c6e";
    hash = "sha256-UTWlpszxXC1sue6E72bIeL+1NKzJ+VC1WQBXOZD0mwI=";
  };

  ignoreCompilationError = false;
}
