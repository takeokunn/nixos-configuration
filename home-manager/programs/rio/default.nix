{ pkgs }: {
  programs.rio = {
    enable = true;
    package = pkgs.rio;
    settings = {
      fonts.size = 20;
      renderer = {
        performance = "High";
        backend = "Automatic";
        target-fps = 120;
      };
      colors = {
        background = "#282A36";
        black = "#44475A";
        blue = "#BD93F9";
        cursor = "#F8F8F2";
        cyan = "#8BE9FD";
        foreground = "#F8F8F2";
        green = "#50fa7b";
        magenta = "#FF79C6";
        red = "#FF5555";
        tabs = "#494c62";
        tabs-active = "#6a6e8e";
        white = "#F8F8F2";
        yellow = "#F1FA8C";
        dim-black = "#393C4b";
        dim-blue = "#AE7BF8";
        dim-cyan = "#69C3dE";
        dim-foreground = "#dfdfd9";
        dim-green = "#06572F";
        dim-magenta = "#FF60BB";
        dim-red = "#FF3C3C";
        dim-white = "#EFEFE1";
        dim-yellow = "#E6A003";
        light-black = "#6272A4";
        light-blue = "#D6ACFF";
        light-cyan = "#A4FFFF";
        light-foreground = "#f8f8f1";
        light-green = "#69FF94";
        light-magenta = "#FF92DF";
        light-red = "#FF6E6E";
        light-white = "#FFFFFF";
        light-yellow = "#FFFFA5";
      };
    };
  };
}
