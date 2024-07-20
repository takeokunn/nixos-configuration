{ pkgs }: {
  home.file = {
    ".config/tmux/tmux.conf".source = ./tmux.conf;
    ".tmux/plugins/tpm/".source = builtins.fetchGit {
      url = "https://github.com/tmux-plugins/tpm";
      rev = "99469c4a9b1ccf77fade25842dc7bafbc8ce9946";
    };
  };

  home.packages = [ pkgs.tmux ];
}
