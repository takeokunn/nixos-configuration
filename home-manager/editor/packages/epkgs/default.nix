{ nurPkgs }:
epkgs:
let
  ai = import ./packages/ai { inherit epkgs; };
  awesome = import ./packages/awesome { inherit epkgs nurPkgs; };
  buffer = import ./packages/buffer { inherit epkgs; };
  client = import ./packages/client { inherit epkgs nurPkgs; };
  coding = import ./packages/coding { inherit epkgs; };
  cursor = import ./packages/cursor { inherit epkgs; };
  dired = import ./packages/dired { inherit epkgs; };
  elfeed = import ./packages/elfeed { inherit epkgs; };
  eshell = import ./packages/eshell { inherit epkgs; };
  eww = import ./packages/eww { inherit epkgs; };
  file = import ./packages/file { inherit epkgs; };
  ime = import ./packages/ime { inherit epkgs nurPkgs; };
  language = import ./packages/language { inherit epkgs nurPkgs; };
  language_specific = import ./packages/language_specific { inherit epkgs nurPkgs; };
  monitor = import ./packages/monitor { inherit epkgs nurPkgs; };
  org = import ./packages/org { inherit epkgs nurPkgs; };
  project = import ./packages/project { inherit epkgs; };
  remote_access = import ./packages/remote_access { inherit nurPkgs; };
  themes = import ./packages/themes { inherit epkgs nurPkgs; };
  search = import ./packages/search { inherit epkgs; };
  terminal = import ./packages/terminal { inherit nurPkgs; };
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
++ terminal
++ tree-sitter
++ window
