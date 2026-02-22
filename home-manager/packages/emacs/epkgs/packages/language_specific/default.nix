{
  nurPkgs,
  epkgs,
  pkgs,
}:
let
  rainbow-csv = nurPkgs.emacs-rainbow-csv;
  emacs-php-doc-block = nurPkgs.emacs-php-doc-block;
  fish-repl = nurPkgs.emacs-fish-repl;
  dasel = nurPkgs.emacs-dasel;
in
with epkgs;
[
  # Basic Lisp
  paredit
  rainbow-delimiters

  # Common Lisp
  sly
  sly-asdf
  sly-macrostep
  sly-named-readtables
  sly-overlay

  # Emacs Lisp
  eros
  elsa
  lispxmp
  macrostep
  elisp-slime-nav
  nameless
  elisp-refs
  highlight-quoted
  highlight-defined

  # Clojure
  anakondo
  cider
  kibit-helper
  clj-refactor
  inf-clojure

  # C/C++
  clang-format

  # Csv
  rainbow-csv

  # JavaScript/TypeScript
  nodejs-repl
  js2-refactor
  jest

  # Ruby
  robe
  rubocop
  ruby-refactor
  inf-ruby
  yard-mode

  # SQL
  sql-indent

  # PHP
  composer
  php-runtime
  psysh
  # laravel-tinker-repl
  emacs-php-doc-block
  phpstan
  phpunit

  # Markdown
  poly-markdown
  markdown-preview-mode

  # Fish
  fish-repl

  # Web
  emmet-mode

  # JSON
  jq-mode
  json-reformat
  dasel

  # Python
  py-isort

  # Dart
  flutter

  # Golang
  gotest
]
