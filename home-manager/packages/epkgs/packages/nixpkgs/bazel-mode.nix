{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "bazel";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "bazelbuild";
    repo = "emacs-bazel-mode";
    rev = "769b30dc18282564d614d7044195b5a0c1a0a5f3";
    hash = "sha256-riB6U1gTuYdStwzejvlsSneMYYFSDvdUFNdaFx3l8BA=";
  };

  ignoreCompilationError = false;
}
