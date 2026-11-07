let
  promptLang = "Reply to the user in Japanese unless requested otherwise. Use merged PRs' language for PR titles and bodies, and existing documents' language for documentation. Use English if none exists. Keep code comments and commit messages in English.";
  defaultModel = "opencode-go/step-5-preview-free";
in
{
  inherit promptLang;

  # Step 5 Preview Free, the single tier for every agent and category, including multimodal-looker:
  # `opencode models --verbose` lists image and video input for it, so no separate vision tier is
  # needed. Its only variants are low, medium, and high, so lanes cannot request max. There is
  # no fallback list since there is only one tier.
  default = {
    model = defaultModel;
  };
}
