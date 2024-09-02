{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "eshell-fringe-status";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "ryuslash";
    repo = "eshell-fringe-status";
    rev = "adc6997c68e39c0d52a2af1b2fd5cf2057783797";
    hash = "sha256-QHJxU+fX6sr0duNEdObRY7cqnfqdGX3mLJQ0KTcjlrM=";
  };

  ignoreCompilationError = false;
}
