{
  pkgs,
  customPackages,
}:
{
  plugins.ts-autotag = {
    enable = true;
  };

  plugins.colorizer = {
    enable = true;
    settings = {
      user_default_options = {
        css = true;
        css_fn = true;
        mode = "background";
      };
    };
  };
}
