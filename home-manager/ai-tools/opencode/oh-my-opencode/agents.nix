{ models }:
{
  # ── Tier 2 (upper): orchestration, implementation, review, multimodal ───
  sisyphus = {
    model = models.upper.model;
    fallback_models = models.upper.fallback;
    variant = "medium";
    ultrawork = {
      model = models.upper.model;
      fallback_models = models.upper.fallback;
      variant = "xhigh";
    };
    compaction = models.compactionCfg;
    prompt_append = models.promptOrchestrator;
    description = "Primary orchestrator. Plans tasks, delegates to specialist agents and categories, ensures quality.";
  };
  hephaestus = {
    model = models.upper.model;
    fallback_models = models.upper.fallback;
    variant = "xhigh";
    allow_non_gpt_model = true;
    compaction = models.compactionCfg;
    prompt_append = models.promptLang;
    description = "Autonomous deep worker. Handles complex multi-file implementations and thorough exploration.";
  };
  oracle = {
    model = models.upper.model;
    fallback_models = models.upper.fallback;
    variant = "xhigh";
    compaction = models.compactionCfg;
    prompt_append = models.promptLang;
    description = "Read-only advisor. Architecture design, code review, and deep debugging analysis.";
  };
  momus = {
    model = models.upper.model;
    fallback_models = models.upper.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Strict reviewer. Thorough critical code and design review.";
  };
  metis = {
    model = models.upper.model;
    fallback_models = models.upper.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Gap detector. Finds overlooked issues, ambiguities, and edge cases.";
  };
  "multimodal-looker" = {
    model = models.upper.model;
    fallback_models = models.upper.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Multimodal analyst. Interprets images, screenshots, diagrams, and visual content.";
  };

  # ── Tier 3 (high): planning, coordination ────────────────────────────────
  prometheus = {
    model = models.high.model;
    fallback_models = models.high.fallback;
    variant = "xhigh";
    compaction = models.compactionCfg;
    prompt_append = models.promptOrchestrator;
    description = "Planning specialist. Creates detailed implementation plans and task breakdowns.";
  };
  atlas = {
    model = models.high.model;
    fallback_models = models.high.fallback;
    variant = "medium";
    compaction = models.compactionCfg;
    prompt_append = models.promptOrchestrator;
    description = "Execution conductor. Splits work into todos, delegates, and consolidates results.";
  };

  # ── Tier 4a (mid): GPT-balanced research/lookup ──────────────────────────
  librarian = {
    model = models.mid.model;
    fallback_models = models.mid.fallback;
    variant = "medium";
    prompt_append = models.promptLibrarian;
    description = "Specification researcher. Looks up docs via context7, web search, and API references.";
  };

  # ── Tier 4b (base): GLM-balanced fast exploration ────────────────────────
  explore = {
    model = models.base.model;
    fallback_models = models.base.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Fast explorer. Quick codebase navigation, file search, and pattern matching.";
  };
}
