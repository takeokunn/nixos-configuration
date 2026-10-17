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

  # One <name>/SKILL.md per command, laid out as an agent-skills source. The files are copied, not
  # linked: agent-skills copies each source with `rsync --safe-links`, which drops a symlink pointing
  # outside the source tree, so a linkFarm's SKILL.md links vanish and the bundle build fails.
  source = pkgs.runCommand "command-skills" { } (
    lib.concatMapStrings (command: ''
      install -Dm644 ${commandPromptToSkill command} "$out/${command}/SKILL.md"
    '') names
  );
}
