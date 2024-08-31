{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "org-volume";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "akirak";
    repo = "org-volume";
    rev = "caa30d5b958c9f37854d7ab35c99445c00bc7d1e";
    hash = "sha256-J1DdP10uc6KeWl+ZhsEBifEJ99lDyKlmcuInHa5p3/M=";
  };

  packageRequires = with epkgs; [ dash request f ];

  ignoreCompilationError = false;
}
