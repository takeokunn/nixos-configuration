{ lib, pkgs }:
{
  programs.swaylock = {
    enable = pkgs.stdenv.isLinux;
    settings = {
      daemonize = true;
      show-failed-attempts = true;
      clock = true;
      screenshot = true;
      effect-blur = "13x13";
      effect-vignette = "0.5:0.5";
      color = "#282A36";
      font = "HackGen";
      indicator = true;
      indicator-radius = 200;
      indicator-thickness = 20;
      line-color = "#44475A";
      ring-color = "#BD93F9";
      inside-color = "#282A36";
      key-hl-color = "#50FA7B";
      separator-color = "#00000000";
      text-color = "#F8F8F2";
      text-caps-lock-color = "#FF79C6";
      line-ver-color = "#BD93F9";
      ring-ver-color = "#BD93F9";
      inside-ver-color = "#282A36";
      text-ver-color = "#8BE9FD";
      ring-wrong-color = "#FF5555";
      text-wrong-color = "#FF5555";
      inside-wrong-color = "#282A36";
      inside-clear-color = "#282A36";
      text-clear-color = "#8BE9FD";
      ring-clear-color = "#8BE9FD";
      line-clear-color = "#8BE9FD";
      line-wrong-color = "#44475A";
      bs-hl-color = "#FFB86C";
      grace = 2;
      grace-no-mouse = true;
      grace-no-touch = true;
      datestr = "%Y/%m/%d";
      timestr = "%H:%m";
      fade-in = 0.4;
      ignore-empty-password = true;
    };
  };
}
