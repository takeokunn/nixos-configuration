{ buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "isucrud";
  version = "1.2.2";

  src = fetchFromGitHub {
    owner = "mazrean";
    repo = "isucrud";
    rev = "refs/tags/v${version}";
    hash = "sha256-XAhkQqUQQIM0wEo4z7o/kHVrelPpgtC8oIfmfEhFbYM=";
  };

  vendorHash = "sha256-XAhkQqUQQIM0wEo4z7o/kHVrelPpgtC8oIfmfEhFbYM=";

  ldflags = [ "-X=main.Version=${version}" ];

  meta = {
    description = "ISUCON用DBへのCRUDへのデータフロー可視化ツール";
    homepage = "https://github.com/mazrean/isucrud";
    mainProgram = "isucrud";
  };
}
