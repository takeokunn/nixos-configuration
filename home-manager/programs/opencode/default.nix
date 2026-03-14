{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
}:
let
  claude-prompts-path = ../../claude-prompts;

  # ── Model tier definitions ─────────────────────────────────────────────────
  # Model IDs must use providerID/modelID format (e.g. "anthropic/claude-sonnet-4-6").

  # GPT (OpenAI): high reasoning, deep coding, fast utility
  gptCodex = "openai/gpt-5.3-codex"; # deep autonomous coding, review

  # GLM (Z.ai): always-available primary for writing/research/visual
  glm = "zai-coding-plan/glm-5";
  glm47 = "zai-coding-plan/glm-4.7";

  # Claude (Anthropic): orchestration flagship + quality fallback
  claudeOpus = "anthropic/claude-opus-4-6";
  claudeSonnet = "anthropic/claude-sonnet-4-6";

  # GitHub Copilot: fast utility for miscellaneous / quick tasks
  copilot = "github-copilot/gpt-4.5";

  # ── Fallback chain policies ────────────────────────────────────────────────
  # claude-sonnet primary: Codex → Opus → GLM
  claudeSonnetFallback = [
    gptCodex
    claudeOpus
    glm
    glm47
  ];
  # gpt-codex primary: Opus → Sonnet → GLM
  gptCodexFallback = [
    claudeOpus
    claudeSonnet
    glm
    glm47
  ];
  # glm-5 primary: stay cheap → Sonnet → Opus only as last resort
  glmFallback = [
    glm47
    claudeSonnet
    claudeOpus
  ];
  # glm-4.7 primary: stay cheap → Sonnet → Opus only as last resort
  glm47Fallback = [
    copilot
    claudeSonnet
  ];
  # copilot primary: fall back to GLM for miscellaneous tasks
  copilotFallback = [
    glm47
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

  opencodeConfig = mcp-servers-nix.lib.mkConfig pkgs {
    flavor = "opencode";
    fileName = "opencode.json";

    programs = {
      serena = {
        enable = true;
        context = "claude-code";
        enableWebDashboard = false;
      };
      context7.enable = true;
      playwright.enable = true;
    };

    settings = {
      theme = "dark";
      plugin = [ "oh-my-opencode" ];
      model = claudeSonnet;
      share = "disabled";

      provider.openai.options = {
        timeout = 600000;
        chunkTimeout = 60000;
      };

      provider.anthropic.options = {
        timeout = 600000;
        chunkTimeout = 60000;
      };

      provider."github-copilot".options = {
        timeout = 600000;
        chunkTimeout = 60000;
      };

      compaction = {
        auto = true;
        prune = true;
        reserved = 10000;
      };

      watcher.ignore = [
        ".devenv/**"
        ".direnv/**"
        ".terraform/**"
        "result/**"
        ".git/**"
        "node_modules/**"
        "flake.lock"
      ];

      servers.deepwiki = {
        type = "http";
        url = "https://mcp.deepwiki.com/mcp";
      };

      permission = {
        bash = {
          "*" = "allow";
          "rm -rf /*" = "deny";
          "rm -rf /" = "deny";
          "sudo rm -rf *" = "deny";
          "dd if=*" = "deny";
          "mkfs.*" = "deny";
          "fdisk *" = "deny";
          "shutdown *" = "deny";
          "reboot *" = "deny";
          "halt *" = "deny";
          "poweroff *" = "deny";
          "killall *" = "deny";
          "pkill -f *" = "deny";
        };
        edit = "allow";
        write = "allow";
        read = "allow";
        glob = "allow";
        grep = "allow";
        webfetch = "allow";
        search = "allow";
        ask = "allow";
        memo = "allow";
        http = "allow";
      };
    };
  };

  tuiConfig = pkgs.writeText "tui.json" (
    builtins.toJSON {
      "$schema" = "https://opencode.ai/tui.json";
      theme = "dracula";
      scroll_speed = 3;
      scroll_acceleration = {
        enabled = true;
      };
      diff_style = "auto";
      keybinds = {
        messages_line_down = "j";
        messages_line_up = "k";
        messages_half_page_down = "ctrl+d";
        messages_half_page_up = "ctrl+u";
        messages_first = "g";
        messages_last = "G";
        messages_next = "]";
        messages_previous = "[";
      };
    }
  );

  ohMyOpencodeConfig = pkgs.writeText "oh-my-opencode.json" (
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
        # ── Orchestrators: planning, delegation, coordination ────────────────
        sisyphus = {
          model = claudeSonnet;
          fallback_models = claudeSonnetFallback;
          variant = "medium";
          ultrawork = {
            model = claudeSonnet;
            fallback_models = claudeSonnetFallback;
            variant = "xhigh";
          };
          compaction = compactionCfg;
          prompt_append = promptOrchestrator;
          description = "Primary orchestrator. Plans tasks, delegates to specialist agents and categories, ensures quality.";
        };
        prometheus = {
          model = glm;
          fallback_models = glmFallback;
          variant = "xhigh";
          compaction = compactionCfg;
          prompt_append = promptOrchestrator;
          description = "Planning specialist. Creates detailed implementation plans and task breakdowns.";
        };
        atlas = {
          model = claudeSonnet;
          fallback_models = claudeSonnetFallback;
          variant = "xhigh";
          compaction = compactionCfg;
          prompt_append = promptOrchestrator;
          description = "Execution conductor. Splits work into todos, delegates, and consolidates results.";
        };
        "multimodal-looker" = {
          model = glm;
          fallback_models = glmFallback;
          variant = "high";
          prompt_append = promptLang;
          description = "Multimodal analyst. Interprets images, screenshots, diagrams, and visual content.";
        };

        # ── Deep workers: autonomous coding, review, reasoning ───────────────
        hephaestus = {
          model = gptCodex;
          fallback_models = gptCodexFallback;
          variant = "xhigh";
          allow_non_gpt_model = true;
          compaction = compactionCfg;
          prompt_append = promptLang;
          description = "Autonomous deep worker. Handles complex multi-file implementations and thorough exploration.";
        };
        oracle = {
          model = gptCodex;
          fallback_models = gptCodexFallback;
          variant = "xhigh";
          compaction = compactionCfg;
          prompt_append = promptLang;
          description = "Read-only advisor. Architecture design, code review, and deep debugging analysis.";
        };
        metis = {
          model = gptCodex;
          fallback_models = gptCodexFallback;
          variant = "xhigh";
          prompt_append = promptLang;
          description = "Gap detector. Finds overlooked issues, ambiguities, and edge cases.";
        };
        momus = {
          model = gptCodex;
          fallback_models = gptCodexFallback;
          variant = "xhigh";
          prompt_append = promptLang;
          description = "Strict reviewer. Thorough critical code and design review.";
        };

        # ── Fast agents: lightweight search, docs lookup ─────────────────────
        librarian = {
          model = claudeSonnet;
          fallback_models = claudeSonnetFallback;
          variant = "high";
          prompt_append = promptLibrarian;
          description = "Specification researcher. Looks up docs via context7, web search, and API references.";
        };
        explore = {
          model = glm;
          fallback_models = [
            glm47
            copilot
          ];
          variant = "high";
          prompt_append = promptLang;
          description = "Fast explorer. Quick codebase navigation, file search, and pattern matching.";
        };
      };

      categories = {
        # ── GLM-primary: natural language, writing, visual, research ──────────
        "visual-engineering" = {
          model = claudeSonnet;
          fallback_models = claudeSonnetFallback;
          prompt_append = promptLang;
          description = "Frontend UI implementation, styling, and component refactors.";
        };
        writing = {
          model = glm47;
          fallback_models = glm47Fallback;
          prompt_append = promptLang;
          description = "Documentation, ADRs, changelogs, and technical writing.";
        };
        research = {
          model = glm47;
          fallback_models = glm47Fallback;
          prompt_append = promptLang;
          description = "Spec lookup, OSS pattern survey, and implementation research.";
        };

        # ── High-tier categories: deep reasoning, coding, review ─────────────
        ultrabrain = {
          model = gptCodex;
          fallback_models = gptCodexFallback;
          variant = "xhigh";
          prompt_append = promptLang;
          description = "Hard reasoning, architecture, tradeoff analysis, and bug forensics.";
        };
        deep = {
          model = gptCodex;
          fallback_models = gptCodexFallback;
          variant = "xhigh";
          prompt_append = promptLang;
          description = "Autonomous deep implementation, non-trivial debugging, and complex multi-file changes.";
        };
        review = {
          model = claudeSonnet;
          fallback_models = claudeSonnetFallback;
          variant = "xhigh";
          prompt_append = promptLang;
          description = "Code review, design review, and debugging analysis.";
        };
        refactor = {
          model = claudeSonnet;
          fallback_models = claudeSonnetFallback;
          variant = "high";
          prompt_append = promptLang;
          description = "Routine refactors, cleanup, repetitive edits, and test generation.";
        };

        # ── Mid-tier categories: general complex work ────────────────────────
        "unspecified-high" = {
          model = glm;
          fallback_models = glmFallback;
          variant = "medium";
          prompt_append = promptLang;
          description = "Default lane for complex general work.";
        };

        # ── Low-tier categories: fast, routine, low-complexity ───────────────
        quick = {
          model = copilot;
          fallback_models = copilotFallback;
          variant = "xhigh";
          prompt_append = promptLang;
          description = "Tiny mechanical edits: typos, trivial renames, one-file micro-fixes.";
        };
        "unspecified-low" = {
          model = copilot;
          fallback_models = copilotFallback;
          variant = "xhigh";
          prompt_append = promptLang;
          description = "Default lane for routine implementation, moderate refactors, and research synthesis.";
        };

        # ── GLM-primary: parallel exploration (rate-limit isolated from OpenAI) ─
        "parallel-explore" = {
          model = glm;
          fallback_models = glmFallback;
          prompt_append = promptLang;
          description = "Parallel read-only exploration: file search, grep, and spec lookup. GLM-primary for rate-limit isolation.";
        };
      };
    }
  );
in
{
  home.packages = [
    nurPkgs.oh-my-openagent
  ];

  home.file.".opencode/CLAUDE.md" = {
    source = "${claude-prompts-path}/CLAUDE.md";
    force = true;
  };

  xdg.configFile."opencode/opencode.json" = {
    source = opencodeConfig;
    force = true;
  };

  xdg.configFile."opencode/oh-my-opencode.json" = {
    source = ohMyOpencodeConfig;
    force = true;
  };

  xdg.configFile."opencode/tui.json" = {
    source = tuiConfig;
    force = true;
  };

  programs.opencode = {
    enable = true;
    package = llmAgentsPkgs.opencode;
    agents = {
      code-quality = builtins.readFile "${claude-prompts-path}/agents/code-quality.md";
      database = builtins.readFile "${claude-prompts-path}/agents/database.md";
      design = builtins.readFile "${claude-prompts-path}/agents/design.md";
      devops = builtins.readFile "${claude-prompts-path}/agents/devops.md";
      docs = builtins.readFile "${claude-prompts-path}/agents/docs.md";
      explore = builtins.readFile "${claude-prompts-path}/agents/explore.md";
      general-purpose = builtins.readFile "${claude-prompts-path}/agents/general-purpose.md";
      git = builtins.readFile "${claude-prompts-path}/agents/git.md";
      performance = builtins.readFile "${claude-prompts-path}/agents/performance.md";
      quality-assurance = builtins.readFile "${claude-prompts-path}/agents/quality-assurance.md";
      security = builtins.readFile "${claude-prompts-path}/agents/security.md";
      test = builtins.readFile "${claude-prompts-path}/agents/test.md";
      validator = builtins.readFile "${claude-prompts-path}/agents/validator.md";
    };
    commands = {
      ask = builtins.readFile "${claude-prompts-path}/commands/ask.md";
      bug = builtins.readFile "${claude-prompts-path}/commands/bug.md";
      define = builtins.readFile "${claude-prompts-path}/commands/define.md";
      define-full = builtins.readFile "${claude-prompts-path}/commands/define-full.md";
      execute = builtins.readFile "${claude-prompts-path}/commands/execute.md";
      execute-full = builtins.readFile "${claude-prompts-path}/commands/execute-full.md";
      feedback = builtins.readFile "${claude-prompts-path}/commands/feedback.md";
      markdown = builtins.readFile "${claude-prompts-path}/commands/markdown.md";
      upstream = builtins.readFile "${claude-prompts-path}/commands/upstream.md";
    };
  };

  home.sessionVariables = {
    OPENCODE_EXPERIMENTAL_BASH_DEFAULT_TIMEOUT_MS = "300000";
    OPENCODE_EXPERIMENTAL_BASH_MAX_TIMEOUT_MS = "1200000";
    OPENCODE_BASH_MAINTAIN_PROJECT_WORKING_DIR = "1";
    OPENCODE_CODE_MAX_OUTPUT_TOKENS = "32000";
    OPENCODE_CODE_AUTO_CONNECT_IDE = "0";
    OPENCODE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "1";
    OPENCODE_CODE_ENABLE_TELEMETRY = "0";
    OPENCODE_CODE_IDE_SKIP_AUTO_INSTALL = "1";
    OPENCODE_CODE_IDE_SKIP_VALID_CHECK = "1";
    MAX_MCP_OUTPUT_TOKENS = "50000";
    MCP_TOOL_TIMEOUT = "120000";
    DISABLE_AUTOUPDATER = "1";
    DISABLE_ERROR_REPORTING = "1";
    DISABLE_INTERLEAVED_THINKING = "1";
    DISABLE_MICROCOMPACT = "1";
    DISABLE_NON_ESSENTIAL_MODEL_CALLS = "1";
    DISABLE_TELEMETRY = "1";
    ENABLE_EXPERIMENTAL_MCP_CLI = "false";
    ENABLE_TOOL_SEARCH = "true";
    OPENCODE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
  };

  programs.serena.ignoredPaths = [
    "**/.devenv/**"
    "**/.direnv/**"
    "**/.terraform/**"
  ];
}
