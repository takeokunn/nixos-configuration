{ models }:
{
  # ── Tier 0 (gpt55Full): ultra-premium, security, architecture ───────────────
  zeus = {
    model = models.gpt55Full.model;
    fallback_models = models.gpt55Full.fallback;
    variant = "xhigh";
    prompt_append = models.promptOrchestrator;
    description = "Ultra orchestrator. Handles the highest-stakes multi-system tasks where quality outweighs speed.";
  };
  themis = {
    model = models.gpt55Full.model;
    fallback_models = models.gpt55Full.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Security specialist. Vulnerability analysis, threat modeling, and security-critical code review requiring exhaustive coverage.";
  };
  daedalus = {
    model = models.gpt55Full.model;
    fallback_models = models.gpt55Full.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Architecture designer. Cross-system, long-horizon design decisions with written rationale. Use oracle for fast advisory; use daedalus for final-say architectural choices.";
  };
  heracles = {
    model = models.gpt55Full.model;
    fallback_models = models.gpt55Full.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Complex debugger. Multi-system root cause analysis spanning services, hard-to-reproduce failures. Use oracle for single-system debug; use heracles for cross-system investigations.";
  };

  # ── Tier 1 (deepseekPro): orchestration, planning, research ─────────────
  sisyphus = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "xhigh";
    prompt_append = models.promptOrchestrator;
    description = "Primary orchestrator. Plans tasks, delegates to specialist agents and categories, ensures quality.";
  };
  atlas = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "xhigh";
    prompt_append = models.promptOrchestrator;
    description = "Execution conductor. Splits work into todos, delegates, and consolidates results.";
  };
  librarian = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "medium";
    prompt_append = models.promptLibrarian;
    description = "Specification researcher. Looks up docs via context7, web search, and API references.";
  };
  explore = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Fast explorer. Quick codebase navigation, file search, and pattern matching.";
  };

  # ── Tier 2 (gptFast): coding, implementation, review, reasoning ──────────
  hephaestus = {
    model = models.gptFast.model;
    fallback_models = models.gptFast.fallback;
    variant = "high";
    allow_non_gpt_model = true;
    prompt_append = models.promptLang;
    description = "Autonomous deep worker. Handles complex multi-file implementations and thorough exploration.";
  };
  oracle = {
    model = models.gptFast.model;
    fallback_models = models.gptFast.fallback;
    variant = "high";
    prompt_append = models.promptLang;
    description = "Read-only advisor. Architecture design, code review, and deep debugging analysis.";
  };
  momus = {
    model = models.gptFast.model;
    fallback_models = models.gptFast.fallback;
    variant = "high";
    prompt_append = models.promptLang;
    description = "Strict reviewer. Thorough critical code and design review.";
  };
  metis = {
    model = models.gptFast.model;
    fallback_models = models.gptFast.fallback;
    variant = "high";
    prompt_append = models.promptLang;
    description = "Gap detector. Finds overlooked issues, ambiguities, and edge cases.";
  };
  "multimodal-looker" = {
    model = models.gptFast.model;
    fallback_models = models.gptFast.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Multimodal analyst. Interprets images, screenshots, diagrams, and visual content.";
  };
  prometheus = {
    model = models.gptFast.model;
    fallback_models = models.gptFast.fallback;
    variant = "high";
    prompt_append = models.promptOrchestrator;
    description = "Planning specialist. Creates detailed implementation plans and task breakdowns.";
  };
}
