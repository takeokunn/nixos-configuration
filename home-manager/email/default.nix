{ pkgs }:
let
  mu = import ./mu { inherit pkgs; };
  offlineimap = import ./offlineimap;
in
[ mu offlineimap ]
