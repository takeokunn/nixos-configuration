# Set1: ZAI primary + GPT mini for hardest tasks
let
  common    = import ./common.nix;
  gpt54mini = "openai/gpt-5.4-mini";
  glm       = "zai-coding-plan/glm-5";
  glm47     = "zai-coding-plan/glm-4.7";
in
common // {
  # Tier 1 — hardest reasoning: GLM primary
  apex = { model = glm;       fallback = [ glm47 gpt54mini ]; };

  # Tier 2 quality — GPT mini primary
  high = { model = gpt54mini; fallback = [ glm glm47 ]; };

  # Tier 2 balanced — GPT mini primary
  mid  = { model = gpt54mini; fallback = [ glm glm47 ]; };

  # Tier 3 — routine/fast
  base = { model = glm47;     fallback = [ glm gpt54mini ]; };
}
