{ epkgs, pkgs, sources }:
let ai = pkgs.callPackage ./plugins.nix { inherit sources epkgs; };
in with epkgs; [ ai.copilot ai.llm ellama ]
