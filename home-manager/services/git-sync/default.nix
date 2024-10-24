{ config, ... }:
let homeDirectory = config.home.homeDirectory;
in {
  services.git-sync = {
    enable = true;
    repositories = {
      "private" = {
        uri = "git@github.com:takeokunn/private";
        path = "${homeDirectory}/ghq/github.com/takeokunn/private";
      };
    };
  };
}
