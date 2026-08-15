{ pkgs, ai-prompts-path }:
let
  inherit (pkgs) lib;

  shared = import ../shared { inherit lib; };

  commandPromptsPath = ai-prompts-path + "/commands";
  agentPromptsPath = ai-prompts-path + "/agents";

  commandFiles = builtins.readDir commandPromptsPath;
  agentFiles = builtins.readDir agentPromptsPath;

  commandNames = map (name: lib.removeSuffix ".md" name) (
    builtins.filter (name: commandFiles.${name} == "regular" && lib.hasSuffix ".md" name) (
      builtins.attrNames commandFiles
    )
  );
  agentNames = map (name: lib.removeSuffix ".md" name) (
    builtins.filter (name: agentFiles.${name} == "regular" && lib.hasSuffix ".md" name) (
      builtins.attrNames agentFiles
    )
  );

  commandPromptToOpencodeCommand =
    command:
    let
      content = builtins.readFile (commandPromptsPath + "/${command}.md");
      parsed = shared.parseFrontmatter content;
      descriptionLine = shared.findLineWithPrefix "description: " parsed.frontmatterLines;
    in
    pkgs.writeText "opencode-command-${command}.md" ''
      ---
      ${descriptionLine}
      ---
      ${parsed.body}
    '';

  agentPromptToOpencodeAgent =
    agent:
    let
      content = builtins.readFile (agentPromptsPath + "/${agent}.md");
      parsed = shared.parseFrontmatter content;
      # opencode's own frontmatter here does not carry the name, but the shape must still be
      # validated as an agent prompt (name: + description:), matching codex's converter. A
      # plain let-binding would never force this assert, since nameLine is otherwise unused
      # and Nix is lazy, so builtins.seq below forces it explicitly.
      nameLine = shared.findLineWithPrefix "name: " parsed.frontmatterLines;
      descriptionLine = shared.findLineWithPrefix "description: " parsed.frontmatterLines;
    in
    builtins.seq nameLine (
      pkgs.writeText "opencode-agent-${agent}.md" ''
        ---
        ${descriptionLine}
        mode: subagent
        ---
        ${parsed.body}
      ''
    );
in
{
  agents = pkgs.linkFarm "opencode-agents" (
    map (agent: {
      name = "${agent}.md";
      path = agentPromptToOpencodeAgent agent;
    }) agentNames
  );

  commands = pkgs.linkFarm "opencode-commands" (
    map (command: {
      name = "${command}.md";
      path = commandPromptToOpencodeCommand command;
    }) commandNames
  );
}
