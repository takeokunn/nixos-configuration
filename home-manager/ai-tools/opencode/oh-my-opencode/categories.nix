{ models }:
{
  # ── DeepSeek-V4-Pro: ultra-complex, security, architecture ────────────────
  ultra = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Default lane for ultra-complex, mission-critical tasks requiring maximum quality.";
  };
  security = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Security-focused analysis, vulnerability review, and threat modeling.";
  };
  architecture = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Cross-system or long-horizon architectural decisions requiring written rationale. Use ultrabrain for single-system tradeoffs; use architecture for irreversible structural choices.";
  };

  # ── Tier 1 (deepseekPro): planning, research, writing ────────────────────
  research = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Spec lookup, OSS pattern survey, and implementation research.";
  };
  writing = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Documentation, ADRs, changelogs, and technical writing.";
  };

  # ── DeepSeek-V4-Pro: coding, implementation, reasoning ───────────────────
  ultrabrain = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Hard reasoning, architecture, tradeoff analysis, and bug forensics.";
  };
  deep = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Autonomous deep implementation, non-trivial debugging, and complex multi-file changes.";
  };
  "unspecified-high" = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Default lane for complex general work.";
  };
  "visual-engineering" = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Frontend UI implementation, styling, and component refactors.";
  };
  refactor = {
    model = models.deepseekPro.model;
    fallback_models = models.deepseekPro.fallback;
    variant = "high";
    prompt_append = models.promptLang;
    description = "Routine refactors, cleanup, repetitive edits, and test generation.";
  };

  # ── Tier 3 (deepseekFlash): quick, trivial tasks ─────────────────────────
  quick = {
    model = models.deepseekFlash.model;
    fallback_models = models.deepseekFlash.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Tiny mechanical edits: typos, trivial renames, one-file micro-fixes.";
  };
  "unspecified-low" = {
    model = models.deepseekFlash.model;
    fallback_models = models.deepseekFlash.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Default lane for routine and trivial implementation tasks.";
  };
}
