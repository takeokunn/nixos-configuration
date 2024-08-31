{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "org-dashboard";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "bard";
    repo = "org-dashboard";
    rev = "02c0699771d199075a286e4502340ca6e7c9e831";
    hash = "sha256-OWwRHMFJvxIKwu+6wdsd+b2zRTX3Q0nbEQi7sl8fIn4=";
  };

  ignoreCompilationError = false;
}
