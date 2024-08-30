{ lib, buildGoModule, fetchFromGitHub }:
buildGoModule rec {
  pname = "ecspresso";
  version = "2.4.0";

  src = fetchFromGitHub {
    owner = "kayac";
    repo = "ecspresso";
    rev = "refs/tags/v${version}";
    hash = "sha256-UX4tKoy0W+uYugRILq9TElz0i1b5hTsh4gDivVVVsZ4=";
  };

  vendorHash = "sha256-TdQPbIgsA9yiQsX71kmocLIVCzd47PLUepA0mdfOjA4=";

  ldflags = [ "-s" "-w" "-X=main.Version=v${version}" ];

  subPackages = [ "cmd/ecspresso" ];

  meta = with lib; {
    description = "A deployment tool for Amazon ECS";
    homepage = "https://github.com/kayac/ecspresso";
    license = licenses.mit;
    mainProgram = "ecspresso";
  };
}
