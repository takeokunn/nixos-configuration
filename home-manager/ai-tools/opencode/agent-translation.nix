{ pkgs, ai-prompts-path }:
let
  inherit (pkgs) lib;

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
      lines = lib.splitString "\n" content;
      descriptionLine = builtins.elemAt lines 2;
      body = lib.concatStringsSep "\n" (lib.drop 4 lines);
    in
    assert lib.hasPrefix "description: " descriptionLine;
    pkgs.writeText "opencode-command-${command}.md" ''
      ---
      ${descriptionLine}
      ---
      ${body}
    '';

  agentPromptToOpencodeAgent =
    agent:
    let
      content = builtins.readFile (agentPromptsPath + "/${agent}.md");
      lines = lib.splitString "\n" content;
      nameLine = builtins.elemAt lines 1;
      descriptionLine = builtins.elemAt lines 2;
      body = lib.concatStringsSep "\n" (lib.drop 4 lines);
    in
    assert lib.hasPrefix "name: " nameLine;
    assert lib.hasPrefix "description: " descriptionLine;
    pkgs.writeText "opencode-agent-${agent}.md" ''
      ---
      ${descriptionLine}
      mode: subagent
      ---
      ${body}
    '';
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
