let
  # ── Model tier definitions ─────────────────────────────────────────────────
  # Model IDs must use providerID/modelID format (e.g. "anthropic/claude-sonnet-4-6").

  # Tier 1 — Main: high reasoning, deep coding, architecture
  gptCodex = "openai/gpt-5.3-codex";

  # Tier 2 — Mid: balanced quality/cost (sonnet:glm ≈ 4:6)
  claudeSonnet = "anthropic/claude-sonnet-4-6";
  glm = "zai-coding-plan/glm-5";

  # Tier 3 — Cheap: fast, routine, low-complexity
  glm47 = "zai-coding-plan/glm-4.7";

  # Unused in main tiers but kept for fallback chains
  claudeOpus = "anthropic/claude-opus-4-6";

  # ── Fallback chain policies ────────────────────────────────────────────────
  # gpt-codex primary: Sonnet → GLM → GLM-4.7
  gptCodexFallback = [
    claudeSonnet
    glm
    glm47
  ];
  # claude-sonnet primary (mid, quality-sensitive): GLM → Codex → GLM-4.7
  claudeSonnetFallback = [
    glm
    gptCodex
    glm47
  ];
  # glm-5 primary (mid, cost-sensitive): Sonnet → GLM-4.7
  glmFallback = [
    claudeSonnet
    glm47
  ];
  # glm-4.7 primary (cheap): GLM-5 → Sonnet
  glm47Fallback = [
    glm
    claudeSonnet
  ];

  # ── Shared prompt_append fragments ─────────────────────────────────────────
  promptLang = "Think and work in English. Reply to the user and write documentation in Japanese.";
  promptOrchestrator = "Assess the full picture, identify task dependencies, and delegate independent tasks in parallel to appropriate subagents. Always specify run_in_background when spawning subagents (false for delegation, true for parallel exploration only). ${promptLang}";
  promptLibrarian = "Verify specifications via Web search and context7 MCP before answering. ${promptLang}";

  # ── Shared compaction model ────────────────────────────────────────────────
  # Context window summarization: needs enough capability to preserve code semantics
  compactionCfg = {
    model = glm;
    variant = "medium";
  };

  # ── Shared provider timeout options ───────────────────────────────────────
  providerTimeoutOpts = {
    timeout = 600000;
    chunkTimeout = 60000;
  };
in
{
  inherit
    gptCodex
    claudeSonnet
    glm
    glm47
    claudeOpus
    gptCodexFallback
    claudeSonnetFallback
    glmFallback
    glm47Fallback
    promptLang
    promptOrchestrator
    promptLibrarian
    compactionCfg
    providerTimeoutOpts
    ;
}
