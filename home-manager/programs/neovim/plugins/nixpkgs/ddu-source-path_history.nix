{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPlugin {
  pname = "ddu-source-path_history";
  version = "2024-09-04";
  src = fetchFromGitHub {
    owner = "Shougo";
    repo = "ddu-source-line";
    rev = "32e88d2e335c8b4346906c03eaca8480bf22fe64";
    hash = "sha256-iKur1PPR99SnZgvy0/aZ+O+1tCX69nGA5Aro62oRG8g=";
  };
}
