{
  programs.aerospace = {
    enable = true;
    config = builtins.readFile ./aerospace.toml;
  };
}
