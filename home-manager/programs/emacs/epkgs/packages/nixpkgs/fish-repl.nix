{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "fish-repl";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "takeokunn";
    repo = "fish-repl.el";
    rev = "5dd66957e494ea201de6e2b0c934dbac6f12743a";
    hash = "sha256-6clzUsB7dllXKe5CeT0kwZl5Cjs9KKhPFaDa9B0aUHE=";
  };

  packageRequires = with epkgs; [ f ];

  ignoreCompilationError = false;
}
