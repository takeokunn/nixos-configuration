import { spawnSync } from "node:child_process"

// Hands each bash command to aitools-rewrite.sh, the rule set the Claude Code and Codex hooks share,
// and runs its rewrite instead when it returns one. Any failure leaves the command as issued.
export const AitoolsRewrite = async () => ({
  "tool.execute.before": async (input, output) => {
    if (input.tool !== "bash" || typeof output.args?.command !== "string") return
    const result = spawnSync("@AITOOLS_REWRITE@", ["plain"], {
      input: output.args.command,
      encoding: "utf8",
      timeout: 5000,
    })
    if (result.status === 0 && result.stdout) output.args.command = result.stdout
  },
})
