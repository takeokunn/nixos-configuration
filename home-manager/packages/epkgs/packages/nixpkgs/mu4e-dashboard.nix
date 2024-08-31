{ fetchFromGitHub, epkgs }:
epkgs.melpaBuild {
  pname = "mu4e-dashboard";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "rougier";
    repo = "mu4e-dashboard";
    rev = "c9c09b7ed6433070de148b656ac273b7fb7cec07";
    hash = "sha256-bCelxaT+qaR2W80Cr591A4cRycIFJmXjeY8/aqIpl5g=";
  };

  packageRequires = with epkgs; [ mu4e async ];

  ignoreCompilationError = false;
}
