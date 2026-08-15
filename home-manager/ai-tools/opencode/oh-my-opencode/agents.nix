{ models }:
let
  inherit (import ./lanes.nix) mkLane;
in
{
  zeus = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptOrchestrator;
    description = "Ultra orchestrator. Handles the highest-stakes multi-system tasks where quality outweighs speed.";
  };
  themis = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Security specialist. Vulnerability analysis, threat modeling, and security-critical code review requiring exhaustive coverage.";
  };
  daedalus = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Architecture designer. Cross-system, long-horizon design decisions with written rationale. Use oracle for fast advisory; use daedalus for final-say architectural choices.";
  };
  heracles = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Complex debugger. Multi-system root cause analysis spanning services, hard-to-reproduce failures. Use oracle for single-system debug; use heracles for cross-system investigations.";
  };

  sisyphus = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptOrchestrator;
    description = "Primary orchestrator. Plans tasks, delegates to specialist agents and categories, ensures quality.";
  };
  atlas = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptOrchestrator;
    description = "Execution conductor. Splits work into todos, delegates, and consolidates results.";
  };
  librarian = mkLane {
    modelTier = models.deepseekPro;
    variant = "medium";
    prompt_append = models.promptLibrarian;
    description = "Specification researcher. Looks up docs via context7, web search, and API references.";
  };
  explore = mkLane {
    modelTier = models.deepseekPro;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Fast explorer. Quick codebase navigation, file search, and pattern matching.";
  };

  hephaestus = mkLane {
    modelTier = models.deepseekPro;
    variant = "high";
    prompt_append = models.promptLang;
    description = "Autonomous deep worker. Handles complex multi-file implementations and thorough exploration.";
    extra = {
      allow_non_gpt_model = true;
    };
  };
  oracle = mkLane {
    modelTier = models.deepseekPro;
    variant = "high";
    prompt_append = models.promptLang;
    description = "Read-only advisor. Architecture design, code review, and deep debugging analysis.";
  };
  momus = mkLane {
    modelTier = models.deepseekPro;
    variant = "high";
    prompt_append = models.promptLang;
    description = "Strict reviewer. Thorough critical code and design review.";
  };
  metis = mkLane {
    modelTier = models.deepseekPro;
    variant = "high";
    prompt_append = models.promptLang;
    description = "Gap detector. Finds overlooked issues, ambiguities, and edge cases.";
  };
  "multimodal-looker" = mkLane {
    modelTier = models.deepseekPro;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Multimodal analyst. Interprets images, screenshots, diagrams, and visual content.";
  };
  prometheus = mkLane {
    modelTier = models.deepseekPro;
    variant = "high";
    prompt_append = models.promptOrchestrator;
    description = "Planning specialist. Creates detailed implementation plans and task breakdowns.";
  };
}
