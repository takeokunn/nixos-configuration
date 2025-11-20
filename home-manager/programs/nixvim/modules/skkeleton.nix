{
  lib,
  config,
  pkgs,
  customPackages,
  ...
}:
let
  cfg = config.plugins.skkeleton;
in
{
  options.plugins.skkeleton = {
    enable = lib.mkEnableOption "skkeleton configuration";

    eggLikeNewline = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable egg-like newline behavior";
    };

    keepState = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Keep skkeleton state";
    };

    sources = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "skk_server" ];
      description = "List of IME sources";
    };

    kanaTable = lib.mkOption {
      type = lib.types.str;
      default = "azik";
      description = "Kana input table (e.g., 'azik')";
    };

    azikTable = lib.mkOption {
      type = lib.types.str;
      default = "us";
      description = "AZIK table variant";
    };

    customKanaTable = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.str);
      default = {
        ss = [ "せい" ];
      };
      description = "Custom kana table mappings";
    };
  };

  config = lib.mkIf cfg.enable {
    extraPlugins = [
      pkgs.vimPlugins.denops-vim
      customPackages.skkeleton
      customPackages.skkeleton-azik
    ];

    extraConfigLua =
      let
        sourcesLua = lib.concatMapStringsSep ", " (s: ''"${s}"'') cfg.sources;
        customTableLua = lib.concatStringsSep ",\n      " (
          lib.mapAttrsToList (
            k: v: ''${k} = { ${lib.concatMapStringsSep ", " (s: ''"${s}"'') v} }''
          ) cfg.customKanaTable
        );
      in
      ''
        vim.fn['skkeleton#config']({
          eggLikeNewline = ${lib.boolToString cfg.eggLikeNewline},
          keepState = ${lib.boolToString cfg.keepState},
          sources = { ${sourcesLua} }
        })

        vim.fn['skkeleton#azik#add_table']('${cfg.azikTable}')
        vim.fn['skkeleton#config']({
          kanaTable = '${cfg.kanaTable}'
        })
        vim.call("skkeleton#register_kanatable", "${cfg.kanaTable}", {
          ${customTableLua}
        })
      '';
  };
}
