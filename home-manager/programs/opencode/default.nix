{
  pkgs,
  nurPkgs,
  llmAgentsPkgs,
  mcp-servers-nix,
}:
let
  claude-prompts-path = ../../claude-prompts;

  # ── Model tier definitions ─────────────────────────────────────────────────
  # GPT (OpenAI): high reasoning, deep coding, fast utility
  gptHigh = "openai/gpt-5.4"; # orchestration, planning, multimodal
  gptCodex = "openai/gpt-5.3-codex"; # deep autonomous coding, review
  gptNano = "openai/gpt-5-nano"; # utility (title generation, compaction, fast tasks)

  # GLM (Z.ai Coding Plan): always-available primary for writing/research/visual
  glm = "zai-coding-plan/glm-5";

  # ── Fallback chain policies ────────────────────────────────────────────────
  # GPT-primary agents: fall through to GLM when OpenAI is unavailable
  gptFallback = [ glm ];
  # GLM-primary agents: fall through to GPT-high for heavier reasoning
  glmFallback = [ gptHigh ];

  # ── Shared compaction model ────────────────────────────────────────────────
  # Context window summarization: needs enough capability to preserve code semantics
  compactionCfg = {
    model = gptNano;
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
      model = gptCodex; # opencode base model (no plugin)
      small_model = gptNano;
      plugin = [ "oh-my-opencode" ];
      share = "disabled";

      provider.openai.options = {
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
      theme = "tokyonight";
      scroll_speed = 3;
      scroll_acceleration = {
        enabled = true;
      };
      diff_style = "auto";
    }
  );

  ohMyOpencodeConfig = pkgs.writeText "oh-my-opencode.json" (
    builtins.toJSON {
      "$schema" = "https://raw.githubusercontent.com/code-yeongyu/oh-my-opencode/dev/assets/oh-my-opencode.schema.json";
      claude_code = {
        agents = false;
      };
      autoupdate = false;
      model_fallback = true;
      disabled_hooks = [ "comment-checker" ];

      agents = {
        # ── Orchestrators (gptHigh): planning, delegation, coordination ───────
        sisyphus = {
          model = gptHigh;
          fallback_models = gptFallback;
          variant = "medium";
          ultrawork = {
            model = gptHigh;
            variant = "xhigh";
          };
          compaction = compactionCfg;
          prompt_append = "状況を俯瞰して捉え、タスクの依存関係を把握し、依存関係のないタスクはできる限りそれぞれ適切なsubagentに並列でタスクを委譲し、検討やプロジェクト品質維持に努めてください。subagentを呼ぶときは run_in_background を必ず指定（委譲は false、並列探索のみ true）、検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "メインのオーケストレータ。計画し、専門エージェントや category ベースの実行へ委譲します。";
        };
        prometheus = {
          model = gptHigh;
          fallback_models = gptFallback;
          variant = "xhigh";
          compaction = compactionCfg;
          prompt_append = "状況を俯瞰して捉え、タスクの依存関係を把握し、依存関係のないタスクはできる限りそれぞれ適切なsubagentに並列でタスクを委譲し、検討やプロジェクト品質維持に努めてください。subagentを呼ぶときは run_in_background を必ず指定（委譲は false、並列探索のみ true）、検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "計画作成役";
        };
        atlas = {
          model = gptHigh;
          fallback_models = gptFallback;
          variant = "xhigh";
          compaction = compactionCfg;
          prompt_append = "状況を俯瞰して捉え、タスクの依存関係を把握し、依存関係のないタスクはできる限りそれぞれ適切なsubagentに並列でタスクを委譲し、検討やプロジェクト品質維持に努めてください。subagentを呼ぶときは run_in_background を必ず指定（委譲は false、並列探索のみ true）、検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "Todo ベースの実行コンダクタ。仕事を切って委譲し、結果を束ねる役。";
        };
        "multimodal-looker" = {
          model = gptHigh;
          fallback_models = gptFallback;
          variant = "high";
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "マルチモーダルの目役";
        };

        # ── Deep workers (gptCodex): autonomous coding, review, reasoning ─────
        hephaestus = {
          model = gptCodex;
          fallback_models = gptFallback;
          variant = "xhigh";
          allow_non_gpt_model = true;
          compaction = compactionCfg;
          prompt_append = "状況を俯瞰して捉え、タスクの依存関係を把握し、依存関係のないタスクはできる限りそれぞれ適切なsubagentに並列でタスクを委譲し、検討やプロジェクト品質維持に努めてください。subagentを呼ぶときは run_in_background を必ず指定（委譲は false、並列探索のみ true）、検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "自律型の deep worker。複数ファイルに跨る難しい実装・深い探索向け。";
        };
        oracle = {
          model = gptCodex;
          fallback_models = gptFallback;
          variant = "xhigh";
          compaction = compactionCfg;
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "read-only の相談役。アーキ設計・コードレビュー・デバッグの深掘り向け。";
        };
        metis = {
          model = gptCodex;
          fallback_models = gptFallback;
          variant = "xhigh";
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "抜け漏れや曖昧さを暴く";
        };
        momus = {
          model = gptCodex;
          fallback_models = gptFallback;
          variant = "xhigh";
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "厳しくレビューする";
        };

        # ── Fast agents (gptNano): lightweight search, docs lookup ───────────
        librarian = {
          model = gptNano;
          fallback_models = gptFallback;
          variant = "high";
          prompt_append = "仕様についてWeb検索することもそうなのですが、context7なども用いて確認してください。検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "context7などで仕様調査";
        };
        explore = {
          model = gptNano;
          fallback_models = gptFallback;
          variant = "high";
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "探索役";
        };
      };

      categories = {
        # ── GLM-primary: natural language, writing, visual, research ──────────
        "visual-engineering" = {
          model = glm;
          fallback_models = glmFallback;
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "Frontend implementation, UI code changes, styling, component refactors.";
        };
        writing = {
          model = glm;
          fallback_models = glmFallback;
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "Documentation, ADRs, changelogs, migration notes, technical writing.";
        };
        research = {
          model = glm;
          fallback_models = glmFallback;
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "Documentation lookup, OSS pattern survey, comparison writeups, implementation research.";
        };

        # ── GPT-Codex-primary: deep reasoning, coding, review ─────────────────
        ultrabrain = {
          model = gptCodex;
          fallback_models = gptFallback;
          variant = "xhigh";
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "Hard reasoning, architecture, large-context review, tradeoff analysis, bug forensics.";
        };
        deep = {
          model = gptCodex;
          fallback_models = gptFallback;
          variant = "xhigh";
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "Autonomous deep implementation, non-trivial debugging, complex multi-file changes.";
        };
        review = {
          model = gptCodex;
          fallback_models = gptFallback;
          variant = "xhigh";
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "Code review, design review, large-context reading, debugging analysis, difficult reasoning.";
        };
        refactor = {
          model = gptCodex;
          fallback_models = gptFallback;
          variant = "high";
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "Routine refactors, cleanup, repetitive edits, code generation, and tests.";
        };

        # ── GPT-High-primary: general complex work ────────────────────────────
        "unspecified-high" = {
          model = gptHigh;
          fallback_models = gptFallback;
          variant = "medium";
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "Default lane for complex general work when the task is not obviously tiny.";
        };

        # ── GPT-Spark-primary: fast, routine, low-complexity ──────────────────
        quick = {
          model = gptNano;
          fallback_models = gptFallback;
          variant = "xhigh";
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "Tiny mechanical edits only: typos, trivial renames, one-file micro-fixes.";
        };
        "unspecified-low" = {
          model = gptNano;
          fallback_models = gptFallback;
          variant = "xhigh";
          prompt_append = "検討や作業は英語で、ユーザーへの回答やドキュメンテーションは日本語でお願いします。";
          description = "Default lane for routine implementation, moderate refactors, research synthesis, and output-heavy general work.";
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
