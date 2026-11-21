{
  config,
  lib,
  pkgs,
  ...
}:
let
  home = config.home.homeDirectory;
in
{
  home.packages = [ pkgs.nautilus ];

  # The folders impermanence keeps under /persist.
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    setSessionVariables = false;
    desktop = "${home}/Desktop";
    documents = "${home}/Documents";
    download = "${home}/Downloads";
    music = "${home}/Music";
    pictures = "${home}/Pictures";
    videos = "${home}/Videos";
    projects = "${home}/Projects";
    # Not persisted: left unset rather than recreated empty after every boot.
    publicShare = null;
    templates = null;
  };

  xdg.mimeApps.defaultApplications = {
    "inode/directory" = [ "org.gnome.Nautilus.desktop" ];
    "text/plain" = [ "emacsclient-gui.desktop" ];
  }
  # Nautilus lists these in its desktop entry and extracts an archive it is asked to open.
  // lib.genAttrs [
    "application/zip"
    "application/x-tar"
    "application/gzip"
    "application/x-compressed-tar"
    "application/x-bzip2-compressed-tar"
    "application/x-xz-compressed-tar"
    "application/zstd"
    "application/x-zstd-compressed-tar"
    "application/x-7z-compressed"
    "application/vnd.rar"
  ] (_: [ "org.gnome.Nautilus.desktop" ]);
}
