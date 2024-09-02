{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "systemd";
  version = "1.0.0";

  files = ''("*.el" "*.txt")'';

  src = fetchFromGitHub {
    owner = "holomorph";
    repo = "systemd-mode";
    rev = "8742607120fbc440821acbc351fda1e8e68a8806";
    hash = "sha256-oj/E+b3oS/2QNNxTYDZ5Zwq/OHKI2FgN/eRV5EAexrE=";
  };

  ignoreCompilationError = false;
}
