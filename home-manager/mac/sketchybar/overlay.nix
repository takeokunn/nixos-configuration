[
  (
    _: prev:
    if prev.stdenv.isDarwin then
      {
        sketchybar = prev.sketchybar.overrideAttrs (old: {
          env = (old.env or { }) // {
            NIX_CFLAGS_COMPILE = "${
              old.env.NIX_CFLAGS_COMPILE or ""
            } -fuse-ld=/Library/Developer/CommandLineTools/usr/bin/ld";
          };
        });
      }
    else
      { }
  )
]
