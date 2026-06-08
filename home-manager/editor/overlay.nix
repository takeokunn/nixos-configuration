{ emacs-overlay }:
[
  (import emacs-overlay)
  (
    _: prev:
    if prev.stdenv.isDarwin then
      {
        emacs = prev.emacs.overrideAttrs (old: {
          buildInputs = (old.buildInputs or [ ]) ++ [ prev.apple-sdk ];
          NIX_CFLAGS_COMPILE = "-std=gnu11 -include stdbool.h";
        });
      }
    else
      { }
  )
]
