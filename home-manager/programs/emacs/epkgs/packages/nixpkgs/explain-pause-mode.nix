{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "explain-pause-mode";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "lastquestion";
    repo = "explain-pause-mode";
    rev = "2356c8c3639cbeeb9751744dbe737267849b4b51";
    hash = "sha256-++znrjiDSx+cy4okFBBXUBkRFdtnE2x+trkmqjB3Njs=";
  };

  ignoreCompilationError = false;
}
