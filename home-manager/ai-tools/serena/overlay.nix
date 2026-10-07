[
  (
    _: prev:
    if prev.stdenv.hostPlatform.isDarwin then
      {
        # nixpkgs marks pywebview broken on every Darwin build. nur-packages' serena
        # overrides pywebview to drop pyside6/qtpy and use the native pyobjc Cocoa
        # backend; that variant builds on Darwin, but it inherits `meta.broken`, so
        # evaluation refuses serena until the flag is cleared here.
        pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
          (_: pyPrev: {
            pywebview = pyPrev.pywebview.overridePythonAttrs (old: {
              meta = old.meta // {
                broken = false;
              };
            });
          })
        ];
      }
    else
      { }
  )
]
