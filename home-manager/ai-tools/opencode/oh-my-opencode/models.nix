let
  promptLang = "Think and work in English. Reply to the user and write documentation in Japanese.";
  gpt55FullModel = "openai/gpt-5.5";
  deepseekProModel = "opencode-go/deepseek-v4-pro";
  gptFastModel = "openai/gpt-5.5-fast";
  deepseekFlashModel = "opencode-go/deepseek-v4-flash";
in
{
  inherit promptLang;
  promptOrchestrator = "Assess the full picture, identify task dependencies, and delegate independent tasks in parallel to appropriate subagents. Always specify run_in_background when spawning subagents (false for delegation, true for parallel exploration only). ${promptLang}";
  promptLibrarian = "Verify specifications via Web search and context7 MCP before answering. ${promptLang}";

  # Tier 0 — ultra-premium, security, architecture: GPT-5.5-Full primary
  gpt55Full = {
    model = gpt55FullModel;
    fallback = [
      gpt55FullModel
    ];
  };

  # Tier 1 — orchestration, planning, research, writing: DeepSeek-V4-Pro primary
  deepseekPro = {
    model = deepseekProModel;
    fallback = [
      deepseekProModel
    ];
  };

  # Tier 2 — coding, implementation, review, reasoning: GPT-5.5-Fast primary
  gptFast = {
    model = gptFastModel;
    fallback = [
      gptFastModel
    ];
  };

  # Tier 3 — quick, trivial tasks: DeepSeek-V4-Flash primary
  deepseekFlash = {
    model = deepseekFlashModel;
    fallback = [
      deepseekFlashModel
    ];
  };
}
