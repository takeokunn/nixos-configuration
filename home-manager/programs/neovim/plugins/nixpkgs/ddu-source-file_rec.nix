{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPlugin {
  pname = "ddu-source-file_rec";
  version = "2024-09-04";
  src = fetchFromGitHub {
    owner = "Shougo";
    repo = "ddu-source-file_rec";
    rev = "3771180decc519a3e09c2c4826740a53997883e1";
    hash = "sha256-IeIjMagARSbjeLgR+u2AfUATPt7xXgyu/iipHt/qHKw=";
  };
}
