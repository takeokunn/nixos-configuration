{ pkgs, neovim-nightly-overlay }: {
  programs.neovim = {
    enable = true;
    package = neovim-nightly-overlay.packages.${pkgs.system}.default;
  };
}
