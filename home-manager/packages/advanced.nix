{ pkgs }:
with pkgs; [
  # for cloud tools
  awscli

  # for network tools
  speedtest-cli

  # for password tools
  pwgen
  (pass.withExtensions (extensions: with extensions; [ pass-otp ]))

  # for DB
  mysql

  # for jokes
  asciiquarium
  cmatrix
  fastfetch
  genact
  sl

  # for ai
  ollama
]
