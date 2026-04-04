let
  promptLang = "Think and work in English. Reply to the user and write documentation in Japanese.";
  gpt54 = "openai/gpt-5.4";
  gptCodex = "openai/gpt-5.3-codex";
  gptCodexSpark = "openai/gpt-5.3-codex-spark";
  gpt54mini = "openai/gpt-5.4-mini";
  glm5 = "zai-coding-plan/glm-5";
  glm47 = "zai-coding-plan/glm-4.7";
in
{
  inherit promptLang;
  promptOrchestrator = "Assess the full picture, identify task dependencies, and delegate independent tasks in parallel to appropriate subagents. Always specify run_in_background when spawning subagents (false for delegation, true for parallel exploration only). ${promptLang}";
  promptLibrarian = "Verify specifications via Web search and context7 MCP before answering. ${promptLang}";

  compactionCfg = {
    model = "zai-coding-plan/glm-5";
    variant = "medium";
  };

  # Tier 1 — hardest reasoning: GPT-5.3-Codex primary
  apex = {
    model = gptCodex;
    fallback = [
      gpt54 # OpenAI: comparable quality
      glm5 # ZAI:    provider diversity
      gptCodexSpark # OpenAI: lighter reasoning
      gpt54mini # OpenAI: last-resort cheapest
    ];
  };

  # Tier 2 — high quality coding/review: GPT-5.4 primary
  upper = {
    model = gpt54;
    fallback = [
      gptCodex # OpenAI: stronger reasoning
      glm5 # ZAI:    provider diversity
      gptCodexSpark # OpenAI: lighter coding
      gpt54mini # OpenAI: last-resort cheapest
    ];
  };

  # Tier 3 — balanced orchestration/planning: GPT-5.4-mini primary
  high = {
    model = gpt54mini;
    fallback = [
      gptCodexSpark # OpenAI: similar-tier coding
      glm5 # ZAI:    equivalent tier
      glm47 # ZAI:    lower last-resort
      gpt54 # OpenAI: up-tier safety net
    ];
  };

  # Tier 4a — GPT-balanced lightweight: GPT-5.3-Codex-Spark primary
  mid = {
    model = gptCodexSpark;
    fallback = [
      glm5 # ZAI:    equivalent tier
      glm47 # ZAI:    lower same-provider
      gpt54mini # OpenAI: similar weight
    ];
  };

  # Tier 4b — GLM-balanced lightweight: GLM-5 primary
  base = {
    model = glm5;
    fallback = [
      glm47 # ZAI:    same-provider lower version
      gptCodexSpark # OpenAI: mid-tier diversity
      gpt54mini # OpenAI: wider OpenAI fallback
    ];
  };
}
