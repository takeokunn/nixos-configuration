{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "eshell-syntax-highlighting";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "akreisher";
    repo = "eshell-syntax-highlighting";
    rev = "26f49633308ea876b5850256e07622de34ad0bdd";
    hash = "sha256-MPypD9p1gUEFGQmq06vCcmuQa9E2dVsVDba6n0fHVNw=";
  };

  ignoreCompilationError = false;
}
