{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "rainbow-csv";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "emacs-vs";
    repo = "rainbow-csv";
    rev = "aa09e2cc79d32f890177c6d0b0376c4aa2708d3e";
    hash = "sha256-5EoBFKgUDLyC9oZDP7vY3Ldk6An39Qo4ngkIHgrB8dU=";
  };

  packageRequires = with epkgs; [ csv-mode ];

  ignoreCompilationError = false;
}
