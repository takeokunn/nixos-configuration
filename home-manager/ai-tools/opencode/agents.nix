{ ai-prompts-path }:
let
  readFiles =
    dir: names:
    builtins.listToAttrs (
      map (name: {
        inherit name;
        value = builtins.readFile "${dir}/${name}.md";
      }) names
    );
in
{
  agents = readFiles "${ai-prompts-path}/agents" [
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
    "verification"
  ];
  commands = readFiles "${ai-prompts-path}/commands" [
    "ask"
    "bug"
    "define"
    "define-full"
    "execute"
    "execute-full"
    "feedback"
    "markdown"
    "remember"
    "simplify"
    "skillify"
    "upstream"
  ];
}
