{
  mkPickKeymap = key: picker: desc: {
    mode = "n";
    key = "<leader>f${key}";
    action.__raw = "function() require('mini.pick').builtin.${picker} end";
    options.desc = desc;
  };

  mkExtraPickKeymap = key: picker: desc: {
    mode = "n";
    key = "<leader>f${key}";
    action.__raw = "function() require('mini.extra').pickers.${picker}() end";
    options.desc = desc;
  };

  mkMiniKeymap = mode: key: module: func: desc: {
    inherit mode key;
    action.__raw = "function() require('mini.${module}').${func}() end";
    options.desc = desc;
    options.silent = true;
  };
}
