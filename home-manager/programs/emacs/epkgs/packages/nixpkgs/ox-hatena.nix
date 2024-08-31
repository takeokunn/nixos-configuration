{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "ox-hatena";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "zonkyy";
    repo = "ox-hatena";
    rev = "24777234566f5472b0e8b3c5faeb2e045fd91e12";
    hash = "sha256-gQ6oU2t/xlyTK4FRInqeHd9AH+vRpCBM/aMbpn1tHTU=";
  };

  ignoreCompilationError = false;
}
