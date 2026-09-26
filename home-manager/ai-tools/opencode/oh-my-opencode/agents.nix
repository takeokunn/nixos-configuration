{ models }:
let
  inherit (import ./lanes.nix) mkLane;
in
# prompt_append values sourced from ./prompts/<name>.md are live prompt text, loaded via
# builtins.readFile, not documentation.
{
  zeus = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append =
      models.promptLang
      + "\n\n"
      + "Identify dependencies and delegate independent work to distinct agents when useful. Set run_in_background explicitly: true only for read-only exploration, false otherwise. Sequence overlapping writes unless separate workspaces prevent conflicts.";
    description = "Orchestrates high-stakes multi-system tasks and delegates independent work.";
  };
  themis = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang;
    description = "Analyzes vulnerabilities, threat models, and security-sensitive code.";
  };
  daedalus = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang;
    description = "Cross-system architecture decisions with written rationale; use oracle for bounded advice.";
  };
  heracles = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang;
    description = "Cross-system root-cause investigations; use oracle for single-system debugging advice.";
  };

  sisyphus = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/sisyphus.md;
    description = "Plans tasks, delegates work, and consolidates results.";
  };
  atlas = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/atlas.md;
    description = "Execution conductor. Splits work into todos, delegates, and consolidates results.";
  };
  librarian = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/librarian.md;
    description = "Specification researcher. Looks up docs via context7, web search, and API references.";
  };
  explore = mkLane {
    modelTier = models.default;
    variant = "high";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/explore.md;
    description = "Fast explorer. Quick codebase navigation, file search, and pattern matching.";
  };

  hephaestus = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/hephaestus.md;
    description = "Implements complex multi-file changes and explores the codebase.";
    extra = {
      allow_non_gpt_model = true;
    };
  };
  oracle = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/oracle.md;
    description = "Read-only advisor. Architecture design, code review, and deep debugging analysis.";
  };
  momus = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/momus.md;
    description = "Reviews plans for execution blockers, reference validity, and final verification readiness.";
  };
  metis = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/metis.md;
    description = "Gap detector. Finds overlooked issues, ambiguities, and edge cases.";
  };
  "multimodal-looker" = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/multimodal-looker.md;
    description = "Multimodal analyst. Interprets images, screenshots, diagrams, and visual content.";
  };
  prometheus = mkLane {
    modelTier = models.default;
    variant = "max";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/prometheus.md;
    description = "Planning specialist. Creates detailed implementation plans and task breakdowns.";
  };
}
