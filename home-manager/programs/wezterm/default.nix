{ pkgs, wezterm-flake }: {
  home.file.".config/wezterm/dracula.toml".source = ./dracula.toml;

  programs.wezterm = {
    enable = true;
    # package = wezterm-flake.packages.${pkgs.system}.default;
    extraConfig = ''
      local wezterm = require("wezterm")

      return {
        use_ime = false,
        hide_tab_bar_if_only_one_tab = true,
        native_macos_fullscreen_mode = false,
        window_padding = {
          left = 5,
          top = 5,
          right = 0,
          bottom = 0,
        },
        color_scheme = "Dracula (Official)",

        font = wezterm.font("HackGen"),
        font_size = 16.0,
        adjust_window_size_when_changing_font_size = false,
        harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }
      }
    '';
  };
}
