{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "view-lock-mode";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "s-fubuki";
    repo = "view-lock-mode";
    rev = "508b1a4b6d5e040455535331244104f5122e340b";
    hash = "sha256-WUhQO/4O0NBpUfNofGUsS7Rfbel8iEGk+qCyOs9o0Ik=";
  };

  ignoreCompilationError = false;
}
