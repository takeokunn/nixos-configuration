{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "sudden-death";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "yewton";
    repo = "sudden-death.el";
    rev = "791a63d3f4df192e71f4232a9a4c5588f4b43dfb";
    hash = "sha256-+h6nWW9upcwWfIvYaF4It38+ouhqeBttm1dVbxpvanw=";
  };

  ignoreCompilationError = false;
}
