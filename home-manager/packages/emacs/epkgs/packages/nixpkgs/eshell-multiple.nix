{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "eshell-multiple";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "takeokunn";
    repo = "eshell-multiple";
    rev = "342c36ef9c71df8738f4435fd4381f506631e7aa";
    hash = "sha256-+4x8Xkaqj44rcvrqv/3M8p+b842c6uLNBGPMaDtQUbs=";
  };

  ignoreCompilationError = false;
}
