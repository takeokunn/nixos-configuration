{ pkgs, ... }:
{
  home.packages = [ pkgs.wget ];
  xdg.configFile."wgetrc".text = ''
    check_certificate = off
    timestamping = on
    no_parent = on
    timeout = 60
    tries = 3
    retry_connrefused = on
    trust_server_names = on
    follow_ftp = on
    adjust_extension = on
    local_encoding = UTF-8
    robots = off
    server_response = on
  '';
}
