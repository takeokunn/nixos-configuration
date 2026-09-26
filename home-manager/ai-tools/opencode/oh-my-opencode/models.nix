let
  promptLang = "Reply to the user in Japanese unless requested otherwise. Write repository documentation, code comments, commit messages, and PR bodies in English.";
  defaultModel = "opencode-go/space-bunny-free";
in
{
  inherit promptLang;

  # Space Bunny Free, the single tier for every agent and category, including multimodal-looker:
  # `opencode models --verbose` lists image and video input for it, so no separate vision tier is
  # needed. Fallback is self-referential since there is only one tier.
  default = {
    model = defaultModel;
    fallback = [
      defaultModel
    ];
  };
}
