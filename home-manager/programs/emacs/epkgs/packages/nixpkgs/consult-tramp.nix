{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "consult-tramp";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "Ladicle";
    repo = "consult-tramp";
    rev = "befa62baca768caa457b167e773b91f1bc7d661f";
    hash = "sha256-Ddeat4FfQS+5BvAP41xNsNw3bzTJv41xuaIA5a44Kvc=";
  };

  packageRequires = with epkgs; [ consult ];

  ignoreCompilationError = false;
}
