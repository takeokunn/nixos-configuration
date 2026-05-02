{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.wget;
  wgetConf = ''
    check_certificate = ${cfg.checkCertificate}
    timestamping = ${cfg.timestamping}
    no_parent = ${cfg.noParent}
    timeout = ${toString cfg.timeout}
    tries = ${toString cfg.tries}
    retry_connrefused = ${cfg.retryConnrefused}
    trust_server_names = ${cfg.trustServerNames}
    follow_ftp = ${cfg.followFtp}
    adjust_extension = ${cfg.adjustExtension}
    local_encoding = ${cfg.localEncoding}
    robots = ${cfg.robots}
    server_response = ${cfg.serverResponse}
  '';
in
with lib;
{
  options.programs.wget = {
    enable = mkEnableOption "Simplistic interactive filtering tool";
    package = mkPackageOption pkgs "wget" { };

    checkCertificate = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        WARNING: This option forces an "insecure" mode of operation that turns the certificate
        verification errors into warnings and allows you to proceed.
      '';
    };

    timestamping = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Use the server-provided last modification date, if available
      '';
    };

    noParent = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Do not go up in the directory structure when downloading recursively
      '';
    };

    timeout = mkOption {
      type = types.ints.unsigned;
      default = 15;
      description = ''
        Wait 60 seconds before timing out. This applies to all timeouts: DNS, connect and read. (The default read timeout is 15 minutes!)
      '';
    };

    tries = mkOption {
      type = types.ints.unsigned;
      default = 20;
      description = ''
        Retry a few times when a download fails, but don’t overdo it. (The default is 20!)
      '';
    };

    retryConnrefused = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Retry even when the connection was refused
      '';
    };

    trustServerNames = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Use the last component of a redirection URL for the local file name
      '';
    };

    followFtp = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Follow FTP links from HTML documents by default
      '';
    };

    adjustExtension = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Add a `.html` extension to `text/html` or `application/xhtml+xml` files that lack one, or a `.css` extension to `text/css` files that lack one
      '';
    };

    localEncoding = mkOption {
      type = types.nullOr types.str;
      default = "UTF-8";
      description = ''
        Use UTF-8 as the default system encoding
        Disabled as it makes `wget` builds that don’t support this feature unusable.
        Does anyone know how to conditionally configure a wget setting?
        http://unix.stackexchange.com/q/34730/6040
      '';
    };

    robots = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Ignore `robots.txt` and `<meta name=robots content=nofollow>`
      '';
    };

    serverResponse = mkOption {
      type = types.nullOr types.str;
      default = "off";
      description = ''
        Print the HTTP and FTP server responses
      '';
    };

    config = mkOption { type = types.lines; };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."wgetrc".text = wgetConf;
  };
}
