{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "flycheck-projectile";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "nbfalcon";
    repo = "flycheck-projectile";
    rev = "ce6e9e8793a55dace13d5fa13badab2dca3b5ddb";
    hash = "sha256-p3/y9iTfgYKJyRNXF2cB7Jam36QWIq4/ZUB+G+YRHjQ=";
  };

  packageRequires = with epkgs; [ flycheck ];

  ignoreCompilationError = false;
}
