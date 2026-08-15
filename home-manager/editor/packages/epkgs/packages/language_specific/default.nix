{ epkgs, nurPkgs }:
let
  rainbow-csv = nurPkgs.emacs-rainbow-csv;
  emacs-php-doc-block = nurPkgs.emacs-php-doc-block;
  fish-repl = nurPkgs.emacs-fish-repl;
  dasel = nurPkgs.emacs-dasel;
  consult-dasel = nurPkgs.emacs-consult-dasel;
  doclive = nurPkgs.emacs-doclive;
in
with epkgs;
[
  paredit
  rainbow-delimiters

  sly
  sly-asdf
  sly-macrostep
  sly-named-readtables
  sly-overlay

  eros
  lispxmp
  macrostep
  elisp-slime-nav
  nameless
  elisp-refs
  highlight-quoted
  highlight-defined

  anakondo
  cider
  kibit-helper
  clj-refactor
  inf-clojure

  clang-format

  rainbow-csv

  nodejs-repl
  js2-refactor
  jest

  robe
  rubocop
  ruby-refactor
  inf-ruby
  yard-mode

  sql-indent

  composer
  php-runtime
  psysh
  emacs-php-doc-block
  phpstan
  phpunit

  poly-markdown
  doclive

  fish-repl

  emmet-mode

  jq-mode
  json-reformat
  dasel
  consult-dasel

  py-isort

  flutter

  gotest
]
