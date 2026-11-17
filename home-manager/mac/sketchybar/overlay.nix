[
  (
    _: prev:
    if prev.stdenv.hostPlatform.isDarwin then
      {
        # nixpkgs' ld64-957.1 crashes (`Trace/BPT trap: 5`) linking sketchybar on macOS 26
        # (Tahoe) and no newer ld64 exists upstream yet, so link with the Xcode Command Line
        # Tools linker. `-fuse-ld=` is a clang driver flag: NIX_LDFLAGS would pass it
        # straight to `ld`, which rejects it, so it goes through NIX_CFLAGS_COMPILE.
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
