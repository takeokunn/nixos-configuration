# Set: GPT+GLM — apex=gpt-5.4, high=gpt-5.3-codex, mid=glm-5, base=glm-5
let
  common = import ./common.nix;
  gpt54 = "openai/gpt-5.4";
  gptCodex = "openai/gpt-5.3-codex";
  gptCodexSpark = "openai/gpt-5.3-codex-spark";
  gpt54mini = "openai/gpt-5.4-mini";
  glm5 = "zai-coding-plan/glm-5";
  glm47 = "zai-coding-plan/glm-4.7";
in
common
// {
  # Tier 1 — hardest reasoning: GPT-5.4 primary, GLM-5 first fallback
  apex = {
    model = gptCodex;
    fallback = [
      glm5
      gptCodex
      gpt54mini
    ];
  };

  # Tier 2 quality — coding/review: GPT-5.3-Codex primary
  high = {
    model = gpt54;
    fallback = [
      gpt54
      glm5
      gpt54mini
    ];
  };

  # Tier 2 balanced — orchestration/planning: GLM-5 primary
  mid = {
    model = gpt54mini;
    fallback = [
      gpt54
      gptCodex
      gpt54mini
    ];
  };

  # Tier 3 — routine/fast: GLM-5 primary, GLM-4.7 first fallback
  base = {
    model = glm5;
    fallback = [
      glm47
      gptCodexSpark
      gpt54mini
    ];
  };
}
