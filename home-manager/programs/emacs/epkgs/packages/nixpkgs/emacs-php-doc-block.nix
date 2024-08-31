{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "php-doc-block";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "moskalyovd";
    repo = "emacs-php-doc-block";
    rev = "bdf1ddba2cadd52ee7dd5691baefc6306ea62c81";
    hash = "sha256-pmCOsKcKcFNZP/ipj5bj9IAK+Ulthso+HQKpzakRCzA=";
  };

  ignoreCompilationError = false;
}
