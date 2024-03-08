{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "ecspresso";
  version = "2.3.2";

  src = fetchFromGitHub {
    owner = "kayac";
    repo = "ecspresso";
    rev = "refs/tags/v${version}";
    hash = "sha256-jqO0kT5DuPeIbHw3AfmbT95mzlS7ovGi2kjJYc4zRyA=";
  };

  vendorHash = "sha256-pfiMKzvGKbfjGfklQjIlaLN1ZRTsln94DKHatDZv+eI=";

  ldflags = [ "-s" "-w" "-X=main.Version=v${version}" ];

  subPackages = [ "cmd/ecspresso" ];

  meta = with lib; {
    description = "A deployment tool for Amazon ECS";
    homepage = "https://github.com/kayac/ecspresso";
    license = licenses.mit;
    mainProgram = "ecspresso";
  };
}
