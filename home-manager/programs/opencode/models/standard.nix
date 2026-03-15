# Set2: GPT + GLM (no Claude)
let
  common = import ./common.nix;

  gptCodex = "openai/gpt-5.3-codex";
  glm      = "zai-coding-plan/glm-5";
  glm47    = "zai-coding-plan/glm-4.7";
in
common // {
  # Tier 1 — hard reasoning, architecture (gptCodex primary)
  apex = {
    model    = gptCodex;
    fallback = [ glm glm47 ];
  };

  # Tier 2 quality — glm replaces claudeSonnet
  high = {
    model    = glm;
    fallback = [ glm47 ];
  };

  # Tier 2 balanced — glm primary
  mid = {
    model    = glm;
    fallback = [ glm47 ];
  };

  # Tier 3 — fast, routine, low-complexity (glm47 primary)
  base = {
    model    = glm47;
    fallback = [ glm ];
  };
}
