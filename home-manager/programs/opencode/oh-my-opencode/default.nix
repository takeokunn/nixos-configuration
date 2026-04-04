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
      "https://raw.githubusercontent.com/code-yeongyu/oh-my-opencode/dev/assets/oh-my-opencode.schema.json";
    claude_code = {
      agents = false;
    };
    autoupdate = false;
    model_fallback = true;
    disabled_hooks = [ "comment-checker" ];

    runtime_fallback = {
      enabled = true;
      retry_on_errors = [
        400
        429
        503
        529
      ];
      max_fallback_attempts = 10;
      cooldown_seconds = 30;
      timeout_seconds = 30;
      notify_on_fallback = true;
    };

    inherit agents categories;
  }
)
