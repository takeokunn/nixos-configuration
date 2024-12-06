{ config, ... }:
{
  services.git-sync = {
    enable = true;
    repositories = {
      private = {
        uri = "git@github.com:takeokunn/private";
        path = "${config.home.homeDirectory}/ghq/github.com/takeokunn/private";
        interval = 600;
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
