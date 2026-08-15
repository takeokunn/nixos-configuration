let
  promptLang = "Think and work in English. Reply to the user and write documentation in Japanese.";
  deepseekProModel = "opencode-go/deepseek-v4-pro";
  deepseekFlashModel = "opencode-go/deepseek-v4-flash";
in
{
  inherit promptLang;
  promptOrchestrator = "Assess the full picture, identify task dependencies, and delegate independent tasks in parallel to appropriate subagents. Always specify run_in_background when spawning subagents (false for delegation, true for parallel exploration only). ${promptLang}";
  promptLibrarian = "Verify specifications via Web search and context7 MCP before answering. ${promptLang}";

  deepseekPro = {
    model = deepseekProModel;
    fallback = [
      deepseekFlashModel
    ];
  };

  deepseekFlash = {
    model = deepseekFlashModel;
    fallback = [
      deepseekProModel
    ];
  };
}
