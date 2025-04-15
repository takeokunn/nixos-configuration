{ pkgs }:
{
  programs.mu = {
    enable = true;
    package = pkgs.mu;
  };

  accounts.email.accounts = {
    Gmail = {
      mu.enable = true;
    };
  };
}
