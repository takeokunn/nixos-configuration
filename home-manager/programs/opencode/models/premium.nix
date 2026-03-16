# Set3: GPT + GLM + Claude (full capability, current config equivalent)
let
  common = import ./common.nix;

  gptCodex = "openai/gpt-5.3-codex";
  claudeSonnet = "anthropic/claude-sonnet-4-6";
  glm = "zai-coding-plan/glm-5";
  glm47 = "zai-coding-plan/glm-4.7";
in
common
// {
  # Tier 1 — hard reasoning, architecture (gptCodex primary)
  apex = {
    model = gptCodex;
    fallback = [
      claudeSonnet
      glm
      glm47
    ];
  };

  # Tier 2 quality — quality-sensitive work (claudeSonnet primary)
  high = {
    model = claudeSonnet;
    fallback = [
      glm
      gptCodex
      glm47
    ];
  };

  # Tier 2 balanced — cost-effective mid work (glm primary)
  mid = {
    model = glm;
    fallback = [
      claudeSonnet
      glm47
    ];
  };

  # Tier 3 — fast, routine, low-complexity (glm47 primary)
  base = {
    model = glm47;
    fallback = [
      glm
      claudeSonnet
    ];
  };
}
