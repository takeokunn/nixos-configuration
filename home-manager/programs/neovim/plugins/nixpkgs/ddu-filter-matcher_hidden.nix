{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPlugin {
  pname = "ddu-filter-matcher_hidden";
  version = "2024-09-04";
  src = fetchFromGitHub {
    owner = "Shougo";
    repo = "ddu-filter-matcher_hidden";
    rev = "61848581edfdcbb9a22fee2d45ee0f6ad5a0469a";
    hash = "sha256-hQGZEUdyvs2Mou43Avqk82nlTRAdm+zJx1c9aj2BXYo=";
  };
}
