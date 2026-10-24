{ lib, ai-prompts-path }:
let
  shared = import ../shared { inherit lib; };

  commandPromptsPath = ai-prompts-path + "/commands";
  agentPromptsPath = ai-prompts-path + "/agents";

  commandFiles = builtins.readDir commandPromptsPath;
  agentFiles = builtins.readDir agentPromptsPath;

  commandNames = builtins.attrNames (lib.filterAttrs (_: type: type == "directory") commandFiles);
  agentNames = map (name: lib.removeSuffix ".md" name) (
    builtins.filter (name: agentFiles.${name} == "regular" && lib.hasSuffix ".md" name) (
      builtins.attrNames agentFiles
    )
  );

  commandPromptToOpencodeCommand =
    command:
    let
      content = builtins.readFile (commandPromptsPath + "/${command}/SKILL.md");
      parsed = shared.parseFrontmatter content;
      descriptionLine = shared.findLineWithPrefix "description: " parsed.frontmatterLines;
    in
    ''
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
      nameLine = shared.findLineWithPrefix "name: " parsed.frontmatterLines;
      descriptionLine = shared.findLineWithPrefix "description: " parsed.frontmatterLines;
      permissionBlock = shared.agentOpencodePermissions parsed.frontmatterLines;
    in
    # opencode drops the name field, so nameLine is forced only to keep
    # findLineWithPrefix's assert as a gate on agents missing `name:`.
    builtins.seq nameLine (''
      ---
      ${descriptionLine}
      mode: subagent
      ${permissionBlock}---
      ${parsed.body}
    '');
in
# Attribute name -> file content, the shape programs.opencode.agents/commands take.
{
  agents = lib.genAttrs agentNames agentPromptToOpencodeAgent;
  commands = lib.genAttrs commandNames commandPromptToOpencodeCommand;
}
