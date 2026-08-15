{ pkgs, ... }:
{
  home.packages = [ pkgs.dust ];
  xdg.configFile."dust/config.toml".text = ''
    reverse=true

    display-full-paths=true

    display-apparent-size=true

    no-colors=true

    no-bars=true

    skip-total=true

    ignore-hidden=true

    output-format="si"
  '';
}
