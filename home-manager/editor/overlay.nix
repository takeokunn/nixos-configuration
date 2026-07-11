{ emacs-overlay }:
[
  (import emacs-overlay)
  (
    _: prev:
    if prev.stdenv.isDarwin then
      {
        emacs = prev.emacs.overrideAttrs (old: {
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
