{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "org-view-mode";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "amno1";
    repo = "org-view-mode";
    rev = "16d7c87a1bef54abd892f0fda2a541043c42e097";
    hash = "sha256-bLRUHFzVA6+04iY1tnmFQzBRgW6P09+gK9it8D8QrVA=";
  };

  ignoreCompilationError = false;
}
