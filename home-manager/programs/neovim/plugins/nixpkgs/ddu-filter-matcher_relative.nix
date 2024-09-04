{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPlugin {
  pname = "ddu-filter-matcher_relative";
  version = "2024-09-04";
  src = fetchFromGitHub {
    owner = "Shougo";
    repo = "ddu-filter-matcher_relative";
    rev = "b94f477541f0e0a3f538041ac321d48ad7e486a5";
    hash = "sha256-T0Qw33r9cQnMXjQozVNaVEHsNJJGK6QmNJp+pNqufyc=";
  };
}
