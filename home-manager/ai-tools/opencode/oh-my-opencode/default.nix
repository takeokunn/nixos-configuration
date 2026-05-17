{
  pkgs,
  models,
}:
let
  agents = import ./agents.nix { inherit models; };
  categories = import ./categories.nix { inherit models; };
in
pkgs.writeText "oh-my-opencode.json" (
  builtins.toJSON {
    "$schema" =
      "https://raw.githubusercontent.com/code-yeongyu/oh-my-openagent/dev/assets/oh-my-opencode.schema.json";
    claude_code = {
      agents = false;
    };
    autoupdate = false;
    model_fallback = true;
    disabled_hooks = [ "comment-checker" ];

    inherit agents categories;
  }
)
