{ models }:
let
  inherit (import ./lanes.nix) mkLane;
in
# prompt_append values sourced from ./prompts/<name>.md are live prompt text, loaded via
# builtins.readFile, not documentation.
{
  zeus = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append =
      models.promptLang
      + "\n\n"
      + "Assess the full picture, identify task dependencies, and delegate independent tasks in parallel to appropriate subagents. Always specify run_in_background when spawning subagents (false for delegation, true for parallel exploration only).";
    description = "Orchestrates high-stakes multi-system tasks and delegates independent work.";
  };
  themis = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Analyzes vulnerabilities, threat models, and security-sensitive code.";
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
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/sisyphus.md;
    description = "Plans tasks, delegates work, and consolidates results.";
  };
  atlas = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/atlas.md;
    description = "Execution conductor. Splits work into todos, delegates, and consolidates results.";
  };
  librarian = mkLane {
    modelTier = models.deepseekFlash;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/librarian.md;
    description = "Specification researcher. Looks up docs via context7, web search, and API references.";
  };
  explore = mkLane {
    modelTier = models.deepseekFlash;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/explore.md;
    description = "Fast explorer. Quick codebase navigation, file search, and pattern matching.";
  };

  hephaestus = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/hephaestus.md;
    description = "Implements complex multi-file changes and explores the codebase.";
    extra = {
      allow_non_gpt_model = true;
    };
  };
  oracle = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/oracle.md;
    description = "Read-only advisor. Architecture design, code review, and deep debugging analysis.";
  };
  momus = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/momus.md;
    description = "Reviews code and design for defects, compatibility risks, and missed requirements.";
  };
  metis = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/metis.md;
    description = "Gap detector. Finds overlooked issues, ambiguities, and edge cases.";
  };
  "multimodal-looker" = mkLane {
    modelTier = models.kimiVision;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/multimodal-looker.md;
    description = "Multimodal analyst. Interprets images, screenshots, diagrams, and visual content.";
  };
  prometheus = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/prometheus.md;
    description = "Planning specialist. Creates detailed implementation plans and task breakdowns.";
  };
}
