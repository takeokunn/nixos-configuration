[
  (
    _: prev:
    if prev.stdenv.isDarwin then
      {
        # nixpkgs' bundled ld64-957.1 crashes (`Trace/BPT trap: 5`, SIGTRAP inside `ld`
        # itself) linking sketchybar's private-framework-heavy binary on macOS 26 (Tahoe).
        # No newer ld64 exists upstream yet, so fall back to the real system linker
        # (installed via Xcode Command Line Tools), which already matches this host's
        # macOS/SDK version and links it without issue.
        # `-fuse-ld=` is a clang *driver* flag, not a raw linker flag: it must go through
        # NIX_CFLAGS_COMPILE (seen by clang's own arg parser), not NIX_LDFLAGS (forwarded
        # straight to `ld`, which then rejects it as an unknown option).
        sketchybar = prev.sketchybar.overrideAttrs (old: {
          env = (old.env or { }) // {
            NIX_CFLAGS_COMPILE = "${old.env.NIX_CFLAGS_COMPILE or ""} -fuse-ld=/Library/Developer/CommandLineTools/usr/bin/ld";
          };
        });
      }
    else
      { }
  )
]
