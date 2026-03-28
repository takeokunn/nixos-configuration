# Set2: GPT + ZAI — cost-balanced (~1/3 GPT, ~2/3 ZAI)
let
  common    = import ./common.nix;
  gptCodex  = "openai/gpt-5.3-codex";
  gpt54mini = "openai/gpt-5.4-mini";
  glm       = "zai-coding-plan/glm-5";
  glm47     = "zai-coding-plan/glm-4.7";
in
common // {
  # Tier 1 — hardest reasoning (S+)
  apex = { model = gptCodex;  fallback = [ glm glm47 ]; };

  # Tier 2 quality — hephaestus, review, visual-engineering
  high = { model = glm;       fallback = [ gpt54mini gptCodex ]; };

  # Tier 2 balanced — orchestration, planning
  mid  = { model = gpt54mini; fallback = [ glm glm47 ]; };

  # Tier 3 — routine/fast
  base = { model = glm47;     fallback = [ glm gpt54mini ]; };
}
