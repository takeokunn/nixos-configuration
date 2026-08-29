{ models }:
let
  inherit (import ./lanes.nix) mkLane;
in
# prompt_append values sourced from ./prompts/<name>.md are live prompt text, loaded via
# builtins.readFile — not documentation.
{
  ultra = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Default lane for ultra-complex, mission-critical tasks requiring maximum quality.";
  };
  security = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Security-focused analysis, vulnerability review, and threat modeling.";
  };
  architecture = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Cross-system or long-horizon architectural decisions requiring written rationale. Use ultrabrain for single-system tradeoffs; use architecture for irreversible structural choices.";
  };

  research = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/research.md;
    description = "Spec lookup, OSS pattern survey, and implementation research.";
  };
  writing = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/writing.md;
    description = "Documentation, ADRs, changelogs, and technical writing.";
  };

  ultrabrain = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/ultrabrain.md;
    description = "Hard reasoning, architecture, tradeoff analysis, and bug forensics.";
  };
  deep = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/deep.md;
    description = "Autonomous deep implementation, non-trivial debugging, and complex multi-file changes.";
  };
  "unspecified-high" = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/unspecified-high.md;
    description = "Default lane for complex general work.";
  };
  "visual-engineering" = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/visual-engineering.md;
    description = "Frontend UI implementation, styling, and component refactors.";
  };
  refactor = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/refactor.md;
    description = "Routine refactors, cleanup, repetitive edits, and test generation.";
  };

  quick = mkLane {
    modelTier = models.deepseekFlash;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/quick.md;
    description = "Tiny mechanical edits: typos, trivial renames, one-file micro-fixes.";
  };
  "unspecified-low" = mkLane {
    modelTier = models.deepseekFlash;
    variant = "xhigh";
    prompt_append = models.promptLang + "\n\n" + builtins.readFile ./prompts/unspecified-low.md;
    description = "Default lane for routine and trivial implementation tasks.";
  };

  artistry = mkLane {
    modelTier = models.deepseekPro;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Complex problem-solving with unconventional, creative approaches - beyond standard patterns.";
  };
}
