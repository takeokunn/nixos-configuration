{ emacs-overlay, ... }:
let overlay = import emacs-overlay;
in [
  overlay
  (final: prev: {
    gotools = prev.gotools.overrideAttrs (old: {
      postPatch = ''
        # The gopls folder contains a Go submodule which causes a build failure
        # and lives in its own package named gopls.
        rm -r gopls
        # getgo is an experimental go installer which adds generic named server and client binaries to $out/bin
        rm -r cmd/getgo
        # remove bundle
        rm -r cmd/bundle
      '';
    });
  })
]
