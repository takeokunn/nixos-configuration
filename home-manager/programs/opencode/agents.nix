{ claude-prompts-path }:
{
  agents = builtins.listToAttrs (
    map
      (name: {
        inherit name;
        value = builtins.readFile "${claude-prompts-path}/agents/${name}.md";
      })
      [
        "code-quality"
        "database"
        "design"
        "devops"
        "docs"
        "explore"
        "general-purpose"
        "git"
        "performance"
        "quality-assurance"
        "security"
        "test"
        "validator"
      ]
  );
  commands = builtins.listToAttrs (
    map
      (name: {
        inherit name;
        value = builtins.readFile "${claude-prompts-path}/commands/${name}.md";
      })
      [
        "ask"
        "bug"
        "define"
        "define-full"
        "execute"
        "execute-full"
        "feedback"
        "markdown"
        "upstream"
      ]
  );
}
