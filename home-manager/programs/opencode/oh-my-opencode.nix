{
  pkgs,
  models,
}:
pkgs.writeText "oh-my-opencode.json" (
  builtins.toJSON {
    "$schema" =
      "https://raw.githubusercontent.com/code-yeongyu/oh-my-opencode/dev/assets/oh-my-opencode.schema.json";
    claude_code = {
      agents = false;
    };
    autoupdate = false;
    model_fallback = true;
    disabled_hooks = [ "comment-checker" ];

    runtime_fallback = {
      enabled = true;
      retry_on_errors = [
        400
        429
        503
        529
      ];
      max_fallback_attempts = 10;
      cooldown_seconds = 30;
      timeout_seconds = 30;
      notify_on_fallback = true;
    };

    agents = {
      # ── Tier 1 (main): deep reasoning, autonomous coding, review ────────────
      hephaestus = {
        model = models.high.model;
        fallback_models = models.high.fallback;
        variant = "xhigh";
        allow_non_gpt_model = true;
        compaction = models.compactionCfg;
        prompt_append = models.promptLang;
        description = "Autonomous deep worker. Handles complex multi-file implementations and thorough exploration.";
      };
      oracle = {
        model = models.high.model;
        fallback_models = models.high.fallback;
        variant = "xhigh";
        compaction = models.compactionCfg;
        prompt_append = models.promptLang;
        description = "Read-only advisor. Architecture design, code review, and deep debugging analysis.";
      };
      metis = {
        model = models.mid.model;
        fallback_models = models.mid.fallback;
        variant = "xhigh";
        prompt_append = models.promptLang;
        description = "Gap detector. Finds overlooked issues, ambiguities, and edge cases.";
      };
      momus = {
        model = models.mid.model;
        fallback_models = models.mid.fallback;
        variant = "xhigh";
        prompt_append = models.promptLang;
        description = "Strict reviewer. Thorough critical code and design review.";
      };

      # ── Tier 2 (mid): orchestration, planning — sonnet primary ──────────────
      sisyphus = {
        model = models.mid.model;
        fallback_models = models.mid.fallback;
        variant = "medium";
        ultrawork = {
          model = models.mid.model;
          fallback_models = models.mid.fallback;
          variant = "xhigh";
        };
        compaction = models.compactionCfg;
        prompt_append = models.promptOrchestrator;
        description = "Primary orchestrator. Plans tasks, delegates to specialist agents and categories, ensures quality.";
      };
      librarian = {
        model = models.high.model;
        fallback_models = models.high.fallback;
        variant = "medium";
        prompt_append = models.promptLibrarian;
        description = "Specification researcher. Looks up docs via context7, web search, and API references.";
      };

      # ── Tier 2 (mid): planning, coordination — glm primary ──────────────────
      prometheus = {
        model = models.mid.model;
        fallback_models = models.mid.fallback;
        variant = "xhigh";
        compaction = models.compactionCfg;
        prompt_append = models.promptOrchestrator;
        description = "Planning specialist. Creates detailed implementation plans and task breakdowns.";
      };
      atlas = {
        model = models.mid.model;
        fallback_models = models.mid.fallback;
        variant = "medium";
        compaction = models.compactionCfg;
        prompt_append = models.promptOrchestrator;
        description = "Execution conductor. Splits work into todos, delegates, and consolidates results.";
      };

      # ── Tier 3 (cheap): fast, lightweight tasks ──────────────────────────────
      explore = {
        model = models.base.model;
        fallback_models = models.base.fallback;
        variant = "medium";
        prompt_append = models.promptLang;
        description = "Fast explorer. Quick codebase navigation, file search, and pattern matching.";
      };
      "multimodal-looker" = {
        model = models.base.model;
        fallback_models = models.base.fallback;
        variant = "medium";
        prompt_append = models.promptLang;
        description = "Multimodal analyst. Interprets images, screenshots, diagrams, and visual content.";
      };
    };

    categories = {
      # ── Tier 1 (main): hard reasoning, architecture ──────────────────────────
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

      # ── Tier 2 (mid): quality-sensitive — sonnet primary ────────────────────
      "visual-engineering" = {
        model = models.high.model;
        fallback_models = models.high.fallback;
        variant = "medium";
        prompt_append = models.promptLang;
        description = "Frontend UI implementation, styling, and component refactors.";
      };
      review = {
        model = models.high.model;
        fallback_models = models.high.fallback;
        variant = "xhigh";
        prompt_append = models.promptLang;
        description = "Code review, design review, and debugging analysis.";
      };

      # ── Tier 2 (mid): general work — glm primary ────────────────────────────
      refactor = {
        model = models.mid.model;
        fallback_models = models.mid.fallback;
        variant = "medium";
        prompt_append = models.promptLang;
        description = "Routine refactors, cleanup, repetitive edits, and test generation.";
      };
      "unspecified-high" = {
        model = models.mid.model;
        fallback_models = models.mid.fallback;
        variant = "medium";
        prompt_append = models.promptLang;
        description = "Default lane for complex general work.";
      };

      # ── Tier 3 (cheap): fast, routine, low-complexity ───────────────────────
      writing = {
        model = models.base.model;
        fallback_models = models.base.fallback;
        variant = "medium";
        prompt_append = models.promptLang;
        description = "Documentation, ADRs, changelogs, and technical writing.";
      };
      research = {
        model = models.base.model;
        fallback_models = models.base.fallback;
        variant = "medium";
        prompt_append = models.promptLang;
        description = "Spec lookup, OSS pattern survey, and implementation research.";
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
        description = "Default lane for routine implementation, moderate refactors, and research synthesis.";
      };
      "parallel-explore" = {
        model = models.base.model;
        fallback_models = models.base.fallback;
        variant = "medium";
        prompt_append = models.promptLang;
        description = "Parallel read-only exploration: file search, grep, and spec lookup. GLM-primary for rate-limit isolation.";
      };
    };
  }
)
