{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "llm";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "ahyatt";
    repo = "llm";
    rev = "dc98688271afa11e1645744240573592b9d8c38a";
    hash = "sha256-w8oHST65VLV1xiUrQfnebn8/IhF3G71GRJGvTkVAR10=";
  };

  packageRequires = with epkgs; [ plz ];

  ignoreCompilationError = false;
}
