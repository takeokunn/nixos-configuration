let
  promptLang = "Think and work in English. Reply to the user and write documentation in Japanese.";
  deepseekProModel = "opencode-go/deepseek-v4-pro";
  deepseekFlashModel = "opencode-go/deepseek-v4-flash";
  kimiVisionModel = "opencode-go/kimi-k2.6";
in
{
  inherit promptLang;

  # DeepSeek-V4-Pro, the default tier for most agents/categories: orchestration, planning, review,
  # and deep implementation. Fallback is the Go-only Flash tier, so it stays within the same
  # (text-only) capability class.
  deepseekPro = {
    model = deepseekProModel;
    fallback = [
      deepseekFlashModel
    ];
  };

  # DeepSeek-V4-Flash, the fast, low-cost tier for routine work (quick/unspecified-low) and
  # lighter-weight lookups (librarian/explore).
  deepseekFlash = {
    model = deepseekFlashModel;
    fallback = [
      deepseekProModel
    ];
  };

  # Kimi-K2.6, vision-capable, for agents that must interpret images (DeepSeek V4 is text-only).
  # No fallback: DeepSeek can't substitute for a vision task, so an outage should fail loudly
  # rather than silently return text-only analysis of an image it never saw.
  kimiVision = {
    model = kimiVisionModel;
    fallback = [ ];
  };
}
