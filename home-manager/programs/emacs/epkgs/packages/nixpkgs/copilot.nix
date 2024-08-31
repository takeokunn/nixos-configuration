{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "copilot";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "copilot-emacs";
    repo = "copilot.el";
    rev = "535ef61e82f09d744cd5b097b1fc99f08cce175c";
    hash = "sha256-/ZDnEZWUFcKnUtFrd/4C7LX16GAdUQncU8ZnYzntKS0=";
  };

  packageRequires = with epkgs; [ f editorconfig ];

  ignoreCompilationError = false;
}
