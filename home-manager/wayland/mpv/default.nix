{ lib, ... }:
{
  programs.mpv.enable = true;
  stylix.targets.mpv.enable = true;

  xdg.mimeApps.defaultApplications = lib.genAttrs [
    "video/mp4"
    "video/x-matroska"
    "video/webm"
    "video/quicktime"
    "audio/mpeg"
    "audio/flac"
    "audio/ogg"
    "audio/mp4"
    "audio/x-wav"
  ] (_: [ "mpv.desktop" ]);
}
