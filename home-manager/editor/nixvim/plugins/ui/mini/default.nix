let
  ui = import ./ui.nix;
  editing = import ./editing.nix;
  navigation = import ./navigation.nix;
  git = import ./git.nix;
  completion = import ./completion.nix;

  allModules = [
    ui
    editing
    navigation
    git
    completion
  ];

  merged =
    builtins.foldl'
      (acc: m: {
        keymaps = acc.keymaps ++ (m.keymaps or [ ]);
        plugins = {
          web-devicons = m.plugins.web-devicons or acc.plugins.web-devicons or { };
          mini = {
            enable = true;
            mockDevIcons = true;
            modules = acc.plugins.mini.modules // (m.plugins.mini.modules or { });
          };
        };
      })
      {
        keymaps = [ ];
        plugins = {
          web-devicons = { };
          mini = {
            enable = true;
            mockDevIcons = true;
            modules = { };
          };
        };
      }
      allModules;
in
merged
