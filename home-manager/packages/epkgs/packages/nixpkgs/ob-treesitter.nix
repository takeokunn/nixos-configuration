{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "ob-treesitter";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "takeokunn";
    repo = "ob-treesitter";
    rev = "c3fac35f95dcaffdb90836c606d119717c43238d";
    hash = "sha256-3lroCj3FhRS1wgb/UVHYO4CjgP1rsicqB/rARvzsfoc=";
  };

  ignoreCompilationError = false;
}
