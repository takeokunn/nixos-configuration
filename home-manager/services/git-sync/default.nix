{ config, ... }:
{
  services.git-sync = {
    enable = false;
    repositories = {
      private = {
        uri = "git@github.com:takeokunn/private";
        path = "${config.home.homeDirectory}/ghq/github.com/takeokunn/private";
        interval = 60 * 60 * 3;
      };
    };
  };

  programs.git = {
    extraConfig = {
      branch.main = {
        sync = true;
        syncNewFiles = true;
        autocommitscript = "git add -A; git commit -m 'Auto-commit changes';";
      };
    };
  };
}
