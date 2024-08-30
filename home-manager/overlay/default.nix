{ emacs-overlay }:
let overlay = import emacs-overlay;
in [
  overlay
  (final: prev: {
    sheldon = prev.sheldon.overrideAttrs (oldAttrs: {
      version = "0.8.0";
      src = prev.fetchFromGitHub {
        owner = "rossmacarthur";
        repo = "sheldon";
        rev = "9836ef98ca2b44f781deafb409028d4dda7fef17";
        # rev = "0.8.0";
        hash = "sha256-eyfIPO1yXvb+0SeAx+F6/z5iDUA2GfWOiElfjn6abJM=";
      };
    });
  })
]
