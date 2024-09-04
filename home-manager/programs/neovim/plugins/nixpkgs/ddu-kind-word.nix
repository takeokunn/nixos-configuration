{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPlugin {
  pname = "ddu-kind-file";
  version = "2024-09-04";
  src = fetchFromGitHub {
    owner = "Shougo";
    repo = "ddu-kind-file";
    rev = "875d00eefe4285fd419d61c12145fc399c7dd67f";
    hash = "sha256-W+Of8WLOVYai5q4rwmRqToEYSLwZCmeyeCJmPZoJnio=";
  };
}
