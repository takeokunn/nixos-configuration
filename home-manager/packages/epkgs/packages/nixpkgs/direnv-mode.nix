{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "direnv";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "wbolster";
    repo = "emacs-direnv";
    rev = "c0bf3b81c7a97e2a0d06d05495e86848254fcc1f";
    hash = "sha256-g500rooMfkVX22u/HNELT+rKv0+q51Xg4k4R48fPyKw=";
  };

  packageRequires = with epkgs; [ dash ];

  ignoreCompilationError = false;
}
