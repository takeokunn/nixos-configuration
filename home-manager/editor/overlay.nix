{ emacs-overlay }:
[
  (import emacs-overlay)
  (
    _: prev:
    if prev.stdenv.isDarwin then
      let
        # mailutils 3.21 fails to link on aarch64-darwin: the libmu_sieve
        # extension modules reference _mu_url_* without linking libmailutils,
        # which the two-level namespace linker rejects. Emacs only needs it at
        # runtime to locate `movemail`, so drop it and let Emacs build its own.
        # Applied as an overlay (not a per-call .override) so that every
        # consumer -- nur-packages, emacs-lsp-booster, emacsPackagesFor -- picks
        # up the same fixed derivation instead of the plain nixpkgs one.
        dropMailutils =
          name: prev.lib.optionalAttrs (prev ? ${name}) {
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
