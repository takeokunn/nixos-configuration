{ pkgs, nurPkgs }:
epkgs:
let
  ai = import ./packages/ai { inherit epkgs pkgs nurPkgs; };
  awesome = import ./packages/awesome { inherit epkgs pkgs nurPkgs; };
  buffer = import ./packages/buffer { inherit epkgs; };
  client = import ./packages/client { inherit epkgs pkgs nurPkgs; };
  coding = import ./packages/coding { inherit epkgs pkgs nurPkgs; };
  cursor = import ./packages/cursor { inherit epkgs; };
  dired = import ./packages/dired { inherit epkgs; };
  elfeed = import ./packages/elfeed { inherit epkgs; };
  eshell = import ./packages/eshell { inherit epkgs pkgs nurPkgs; };
  eww = import ./packages/eww { inherit epkgs; };
  file = import ./packages/file { inherit epkgs; };
  ime = import ./packages/ime { inherit epkgs; };
  language = import ./packages/language { inherit epkgs pkgs nurPkgs; };
  language_specific = import ./packages/language_specific { inherit epkgs pkgs nurPkgs; };
  monitor = import ./packages/monitor { inherit epkgs pkgs nurPkgs; };
  org = import ./packages/org { inherit epkgs pkgs nurPkgs; };
  project = import ./packages/project { inherit epkgs; };
  remote_access = import ./packages/remote_access { inherit epkgs pkgs nurPkgs; };
  themes = import ./packages/themes { inherit epkgs nurPkgs; };
  search = import ./packages/search { inherit epkgs; };
  tree-sitter = import ./packages/tree-sitter { inherit epkgs; };
  window = import ./packages/window { inherit epkgs; };
in
ai
++ awesome
++ buffer
++ client
++ coding
++ cursor
++ dired
++ elfeed
++ eshell
++ eww
++ file
++ ime
++ language
++ language_specific
++ monitor
++ org
++ project
++ remote_access
++ themes
++ search
++ tree-sitter
++ window
