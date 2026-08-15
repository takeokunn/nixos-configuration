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

    # print sizes in powers of 1000 (e.g., 1.1G)
    output-format="si"
  '';
}
