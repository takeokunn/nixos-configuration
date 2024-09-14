{ epkgs, pkgs, sources }:
let packages = pkgs.callPackage ./packages.nix { inherit epkgs sources; };
in with epkgs; [
  apache-mode
  packages.bazel-mode
  bison-mode
  cask-mode
  cfn-mode
  clojure-mode
  cmake-mode
  coffee-mode
  crontab-mode
  csharp-mode
  csv-mode
  cuda-mode
  crystal-mode
  dart-mode
  dhall-mode
  packages.direnv-mode
  docker-compose-mode
  dockerfile-mode
  dotenv-mode
  elixir-mode
  elm-mode
  fish-mode
  forth-mode
  fsharp-mode
  git-modes
  glsl-mode
  go-mode
  gradle-mode
  graphql-mode
  graphviz-dot-mode
  groovy-mode
  hack-mode
  haskell-mode
  hcl-mode
  hy-mode
  ini-mode
  jade-mode
  js2-mode
  json-mode
  jsonnet-mode
  kotlin-mode
  lua-mode
  markdown-mode
  mermaid-mode
  nasm-mode
  neon-mode
  nim-mode
  ninja-mode
  nix-mode
  nginx-mode
  nov
  pcap-mode
  php-mode
  phpt-mode
  plantuml-mode
  protobuf-mode
  pug-mode
  prisma-mode
  processing-mode
  python-mode
  qt-pro-mode
  robots-txt-mode
  rust-mode
  scala-mode
  scad-mode
  scss-mode
  slim-mode
  solidity-mode
  ssh-config-mode
  swift-mode
  syslog-mode
  packages.systemd-mode
  terraform-mode
  toml-mode
  tmux-mode
  typescript-mode
  v-mode
  vue-mode
  vimrc-mode
  wat-mode
  web-mode
  packages.web-php-blade-mode
  wolfram-mode
  yaml-mode
  yarn-mode
  zig-mode
]
