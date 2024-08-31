{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "laravel-tinker-repl";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "takeokunn";
    repo = "laravel-tinker-repl.el";
    rev = "2c4256eb4ca4c636efbf0164a201bfd86719d651";
    hash = "sha256-ETl7fbMfyu0B+UvAcwg18ls8BZRR/PMxiF/cZPum+NQ=";
  };

  packageRequires = with epkgs; [ f php-mode ];

  ignoreCompilationError = false;
}
