{ pkgs, sources }:
epkgs:
let
  ai = import ./packages/ai { inherit epkgs; };
  awesome = import ./packages/awesome { inherit epkgs pkgs sources; };
  buffer = import ./packages/buffer { inherit epkgs; };
  client = import ./packages/client { inherit epkgs pkgs sources; };
  coding = import ./packages/coding { inherit epkgs pkgs sources; };
  cursor = import ./packages/cursor { inherit epkgs pkgs; };
  dired = import ./packages/dired { inherit epkgs; };
  elfeed = import ./packages/elfeed { inherit epkgs; };
  eshell = import ./packages/eshell { inherit epkgs pkgs sources; };
  eww = import ./packages/eww { inherit epkgs; };
  exwm = import ./packages/exwm { inherit epkgs; };
  file = import ./packages/file { inherit epkgs; };
  ime = import ./packages/ime { inherit epkgs; };
  language = import ./packages/language { inherit epkgs pkgs sources; };
  language_specific = import ./packages/language_specific { inherit epkgs pkgs sources; };
  monitor = import ./packages/monitor { inherit epkgs pkgs sources; };
  org = import ./packages/org { inherit epkgs pkgs sources; };
  project = import ./packages/project { inherit epkgs; };
  remote_access = import ./packages/remote_access { inherit epkgs pkgs sources; };
  themes = import ./packages/themes { inherit epkgs; };
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
++ exwm
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
