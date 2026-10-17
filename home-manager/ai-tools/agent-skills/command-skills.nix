{ pkgs }:
let
  inherit (pkgs) lib;

  shared = import ../shared { inherit lib; };

  commandPromptsPath = ../ai-prompts/commands;
  commandFiles = builtins.readDir commandPromptsPath;

  names = map (name: lib.removeSuffix ".md" name) (
    builtins.filter (name: commandFiles.${name} == "regular" && lib.hasSuffix ".md" name) (
      builtins.attrNames commandFiles
    )
  );

  # Slash commands carry extra frontmatter that skills do not accept; keep name and description.
  commandPromptToSkill =
    command:
    let
      parsed = shared.parseFrontmatter (builtins.readFile (commandPromptsPath + "/${command}.md"));
      descriptionLine = shared.findLineWithPrefix "description: " parsed.frontmatterLines;
    in
    pkgs.writeText "command-skill-${command}.md" ''
      ---
      name: ${command}
      ${descriptionLine}
      ---
      ${parsed.body}
    '';
in
{
  inherit names;

  # One <name>/SKILL.md per command, laid out as an agent-skills source.
  source = pkgs.linkFarm "command-skills" (
    map (command: {
      name = "${command}/SKILL.md";
      path = commandPromptToSkill command;
    }) names
  );
}
