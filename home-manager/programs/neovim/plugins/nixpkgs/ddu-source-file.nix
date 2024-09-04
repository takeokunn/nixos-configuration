{ vimUtils, fetchFromGitHub }:
vimUtils.buildVimPlugin {
  pname = "ddu-source-file";
  version = "2024-09-04";
  src = fetchFromGitHub {
    owner = "Shougo";
    repo = "ddu-source-file";
    rev = "db0510a1b19d5d9e7d02214884a796914acb495d";
    hash = "sha256-74gcMfnUFVjPsKlpPMmt5jyQdqlZRfGyGxnwek0MyNw=";
  };
}
