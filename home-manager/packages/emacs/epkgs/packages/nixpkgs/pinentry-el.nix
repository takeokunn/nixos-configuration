{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "pinentry";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "ueno";
    repo = "pinentry-el";
    rev = "a6441224da04656370e993e2616185cc31afaff9";
    hash = "sha256-xgzdwLEsjjkyPB6z1WM/N+rZNbndKXMPNUZ7Uaa8zT8=";
  };

  ignoreCompilationError = false;
}
