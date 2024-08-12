{ pkgs }: {
  home.file = {
    ".tigrc".source = ./.tigrc;
    ".tig/dracula/".source = builtins.fetchGit {
      url = "https://github.com/dracula/tig";
      rev = "e8a3387d8353e90cca41f5d89c3e1f74f1f7c8c6";
    };
  };
  home.packages = with pkgs; [ tig ];
}
