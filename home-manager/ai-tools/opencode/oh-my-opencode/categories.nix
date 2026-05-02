{ models }:
{
  # ── Tier 1 (apex): hard reasoning, architecture ──────────────────────────
  ultrabrain = {
    model = models.apex.model;
    fallback_models = models.apex.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Hard reasoning, architecture, tradeoff analysis, and bug forensics.";
  };
  deep = {
    model = models.apex.model;
    fallback_models = models.apex.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Autonomous deep implementation, non-trivial debugging, and complex multi-file changes.";
  };

  # ── Tier 2 (upper): code/review quality ──────────────────────────────────
  review = {
    model = models.upper.model;
    fallback_models = models.upper.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Code review, design review, and debugging analysis.";
  };

  # ── Tier 3 (high): balanced general work ─────────────────────────────────
  "unspecified-high" = {
    model = models.high.model;
    fallback_models = models.high.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Default lane for complex general work.";
  };
  "visual-engineering" = {
    model = models.high.model;
    fallback_models = models.high.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Frontend UI implementation, styling, and component refactors.";
  };

  # ── Tier 4a (mid): GPT-balanced research/routine ─────────────────────────
  research = {
    model = models.mid.model;
    fallback_models = models.mid.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Spec lookup, OSS pattern survey, and implementation research.";
  };
  refactor = {
    model = models.mid.model;
    fallback_models = models.mid.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Routine refactors, cleanup, repetitive edits, and test generation.";
  };

  # ── Tier 4b (base): GLM-balanced routine/fast ────────────────────────────
  writing = {
    model = models.base.model;
    fallback_models = models.base.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Documentation, ADRs, changelogs, and technical writing.";
  };
  quick = {
    model = models.base.model;
    fallback_models = models.base.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Tiny mechanical edits: typos, trivial renames, one-file micro-fixes.";
  };
  "unspecified-low" = {
    model = models.base.model;
    fallback_models = models.base.fallback;
    variant = "xhigh";
    prompt_append = models.promptLang;
    description = "Default lane for routine implementation and moderate refactors.";
  };
  "parallel-explore" = {
    model = models.base.model;
    fallback_models = models.base.fallback;
    variant = "medium";
    prompt_append = models.promptLang;
    description = "Parallel read-only exploration: file search, grep, and spec lookup. GLM-primary for rate-limit isolation.";
  };
}
