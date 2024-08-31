{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "web-php-blade-mode";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "takeokunn";
    repo = "web-php-blade-mode";
    rev = "a4463d2732caa8c3650826ee4fc79f3fd29c9e56";
    hash = "sha256-hZKMck0xJWTub81yCupCie+Z8FFFFP26IRm7uN/mbTI=";
  };

  ignoreCompilationError = false;
}
