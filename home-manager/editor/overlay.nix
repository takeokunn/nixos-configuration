{ emacs-overlay }:
[
  (import emacs-overlay)
  (
    _: prev:
    if prev.stdenv.isDarwin then
      let
        dropMailutils =
          name:
          prev.lib.optionalAttrs (prev ? ${name}) {
            ${name} = prev.${name}.override { withMailutils = false; };
          };
      in
      dropMailutils "emacs-unstable"
      // dropMailutils "emacs-git"
      // {
        emacs = (prev.emacs.override { withMailutils = false; }).overrideAttrs (old: {
          buildInputs = (old.buildInputs or [ ]) ++ [ prev.apple-sdk ];
          env = (old.env or { }) // {
            NIX_CFLAGS_COMPILE = "${old.env.NIX_CFLAGS_COMPILE or ""} -std=gnu11 -include stdbool.h";
          };
        });
      }
    else
      { }
  )
]
