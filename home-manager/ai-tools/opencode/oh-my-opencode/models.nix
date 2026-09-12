let
  promptLang = "Think and work in English. Reply to the user and write documentation in Japanese.";
  deepseekModel = "opencode-go/deepseek-v4.1-flash";
in
{
  inherit promptLang;

  # DeepSeek-V4.1-Flash, the single tier for every agent and category, including multimodal-looker.
  # The prior V4 generation was text-only, requiring a separate Kimi vision tier; V4.1 Flash is
  # claimed to handle image input directly (per efoo-team/opencode-setting's opencode-go_deepseek
  # formation), but that capability is unverified from this repo. Fallback is self-referential
  # since there is only one tier.
  deepseek = {
    model = deepseekModel;
    fallback = [
      deepseekModel
    ];
  };
}
