{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPlugin {
  pname = "ddu-filter-matcher_substring";
  version = "2024-09-04";
  src = fetchFromGitHub {
    owner = "Shougo";
    repo = "ddu-filter-matcher_substring";
    rev = "d6defcd1f4a87cc717a72cc620307f9b1e11290d";
    hash = "sha256-NDN3NIzM8eD+BU+6vl4grKXV0uvpAp30ziSuobfgkDs=";
  };
}
