---
argument-hint: [instruction1, instruction2, ...] | apply [worktree-path]
description: Race N candidate implementations in parallel worktrees; you pick, one /design-sync on apply
---

<purpose>
Generates N independent candidate implementations in parallel, isolated git worktrees, then presents them
  side by side with no automated ranking: you pick, and exactly one explicit apply step runs `/design-sync`
  and writes the winner back. Equivalent to Cursor's `/best-of-n` (verified against cursor.com/docs: N
  parallel isolated-worktree runs, no auto-judge, no auto-merge, an explicit apply-worktree step), adapted for
  `/design-sync` because Claude Code has no mechanism for one command to invoke another slash command
  programmatically.
</purpose>

<rules priority="critical">
  <rule>Claude Code has no `SlashCommand` tool and no documented way for this command's body to call
    `/design-sync` directly (verified against code.claude.com/docs during specification). The only buildable
    mechanism is a fresh, non-interactive `claude -p` subprocess whose own prompt asks it to run `/design-sync`
    itself. Do not attempt any other invocation shape.</rule>
  <rule>`/design-sync` is invoked at most once per run of this command, in the `apply` phase only, for the one
    chosen candidate. The `generate` phase's subprocesses never call `/design-sync` — they perform the
    instruction's design/implementation work locally, inside their own throwaway worktree, and stop there.
    Worktree isolation only isolates local git state; it does nothing to isolate a call to the shared remote
    Claude Design project, and `/design-sync`'s real concurrency behavior is undocumented anywhere reachable
    (checked: code.claude.com/docs, support.claude.com, this repo). Do not "fix" this by having generate call
    `/design-sync` N times in parallel and relying on worktrees for safety — that isolates the wrong layer.</rule>
  <rule>Before creating any worktree, detect whether `/design-sync` exists in the current session by checking
    this session's own connected MCP servers/tools for the `claude-design` namespace (this session already
    knows this without spawning anything). If it's absent, stop immediately with a clear message naming what's
    missing (the `claude-design` MCP server, or `/design-login` not yet run) rather than creating worktrees
    that will only fail downstream. This is the graceful-failure path for opencode and Codex, which
    auto-discover this file with no way to exclude it (no CLI-scoping mechanism exists in this repo).</rule>
  <rule>Never commit to the default branch, and never mutate shared working-tree state (the invoking session's
    own checkout) to run a candidate. Every candidate gets its own `git worktree add`, never the current one.
    SSOT-EXEMPT: restated because the failure is irreversible.</rule>
</rules>
<rules priority="important">
  <rule>Pass each instruction through to its subprocess opaquely. Never construct `/design-sync`'s own
    arguments on the caller's behalf: its argument syntax, output shape, and idempotency are undocumented
    (verified absent from Anthropic's own docs during specification), so guessing a shape here is guessing at
    an external contract this command cannot check.</rule>
  <rule>Whether user-installed slash commands execute correctly when embedded as literal text inside a
    `claude -p` prompt, and whether N such subprocesses nest safely when launched concurrently from inside an
    already-running Claude Code session, are both undocumented (checked: code.claude.com/docs/en/headless.md,
    agent-view.md). State this caveat in the output every time, regardless of how many times this command has
    run before — there is no persistent record of prior runs to check, so don't claim to know whether this is
    a first run.</rule>
  <rule>No automated judging step exists or should be added: no agent or skill in this corpus scores a visual
    artifact as evidence (everything here cites file:line or command output), and Cursor's own `/best-of-n`
    does not auto-judge either (verified: cursor.com/docs states you manually select the result). Present the
    N candidates' local diffs and subprocess logs; do not recommend a winner.</rule>
  <rule>Never build the subprocess command line by interpolating the raw instruction text into a string and
    `eval`-ing it: an instruction containing a quote character followed by shell metacharacters then executes
    as a second command with the orchestrating session's own privileges, not the subprocess's restricted ones.
    Pass the instruction as a shell variable and let the shell expand it directly (`claude -p "$INSTRUCTION"
    ...`), or write it to a file inside that candidate's own worktree first and reference the file — never
    concatenate untrusted text into a command string that gets re-parsed.</rule>
  <rule>There is no cap on N anywhere in this command: it is the length of whatever list the user provides.
    Each candidate is a full nested Claude Code session with its own API usage. Before isolating, if N is
    large enough that the cost is likely non-obvious to the user (there's no fixed threshold documented
    anywhere to cite here — use judgment and say why), confirm with AskUserQuestion before spawning
    anything.</rule>
  <rule>PreToolUse hooks (`guard-and-guide`, `block-destructive-git`, `block-bare-cd`) still apply to each
    subprocess: they fire regardless of permission mode. Don't loosen them thinking a headless subprocess is
    unguarded — they're the actual backstop behind `--permission-mode acceptEdits --permission-prompts none`,
    not a decoration this command's own instructions substitute for.</rule>
  <rule>Follows output_discipline in CLAUDE.md for everything this command writes.</rule>
</rules>

<workflow>
  <phase name="prepare">
    <step order="1">
      <action>Parse the argument. Default mode: a comma-separated list of instructions, one per candidate
        (count N = list length; repeat one instruction verbatim N times for an "identical instruction, rely on
        run-to-run variation" run instead of N distinct instructions). Apply mode: the literal word `apply`
        followed by a worktree path from a prior run of this command in the same session.</action>
      <output>Mode (generate or apply) and the parsed instruction list or worktree path</output>
    </step>
    <step order="2">
      <action>Generate mode only: check this session's own available MCP tools/servers for the `claude-design`
        namespace (do not spawn a subprocess to check this — the orchestrating session already knows). Absent:
        stop now, per the critical rule above, and report which piece is missing.</action>
      <output>Availability confirmed, or the command exits here with a named reason</output>
    </step>
  </phase>

  <phase name="isolate" condition="mode == generate">
    <step order="1">
      <action>For each of the N instructions, create an isolated git worktree off the current HEAD:
        `git worktree add -b design-best-of-n/&lt;n&gt;-&lt;short-sha&gt; &lt;path&gt; HEAD`, one per candidate, never
        reusing or touching the invoking session's own checkout.</action>
      <tool>Bash</tool>
      <output>N worktree paths and branch names</output>
    </step>
  </phase>

  <phase name="generate" condition="mode == generate">
    <step order="1">
      <action>For each worktree, hold that candidate's instruction in a shell variable (never interpolate it
        into a string that then gets `eval`-ed) and spawn `claude -p "$INSTRUCTION" --permission-mode
        acceptEdits --permission-prompts none` (documented headless flags; verified against
        code.claude.com/docs/en/headless.md and permission-modes.md during specification), `cd`'d into that
        worktree, backgrounded so all N run concurrently. The instruction is the candidate's design/
        implementation task only — it does not mention `/design-sync`; per the critical rule above, syncing
        happens once, later, in `apply`. Capture each subprocess's stdout/stderr to a log file inside its own
        worktree. `timeout` is not a bare shell builtin on macOS; resolve it via `nix run nixpkgs#coreutils --
        timeout` (or equivalent) rather than assuming it exists.</action>
      <tool>Bash (run_in_background per subprocess)</tool>
      <output>N background task handles and their log file paths</output>
    </step>
    <step order="2">
      <action>Wait for every subprocess to finish or fail; do not proceed to present with any still
        running.</action>
      <output>Per-candidate exit status</output>
    </step>
  </phase>
  <reflection_checkpoint id="generation_quality" after="generate">
    <check>At least one candidate exited successfully with a non-empty local diff. All N failing, or all N
      producing no change, means present would show an empty comparison — report that plainly as a failed run,
      don't proceed to present a table with nothing useful in it.</check>
    <check>Every candidate's log file actually exists and is non-empty, distinguishing a subprocess that ran
      and produced no output from one that never started.</check>
    <on_unmet>Report which candidates failed and how (log tail), and stop before present rather than showing an
      empty or misleading table.</on_unmet>
  </reflection_checkpoint>

  <phase name="present" condition="mode == generate">
    <step order="1">
      <action>For each candidate, in its own worktree, run `git status --porcelain` and `git diff --stat` to
        show what actually changed locally, and tail the subprocess log. Print a comparison table: candidate
        number, worktree path, instruction used, exit status, files changed, log path. Do not rank, score, or
        recommend one — per the important rule above, that judgment has no evidence-tiered basis in this
        corpus for a visual artifact.</action>
      <output>N-row comparison table, worktree paths preserved for a later apply call</output>
    </step>
  </phase>

  <phase name="apply" condition="mode == apply">
    <step order="1">
      <action>Confirm the named worktree path exists and still carries uncommitted or committed candidate
        changes (`git -C &lt;path&gt; status`). If it doesn't exist or was already cleaned up, stop with a clear
        message rather than guessing which candidate was meant.</action>
      <output>Confirmed target worktree</output>
    </step>
    <step order="2">
      <action>This is the one point in the whole command where a real sync-back may happen. Ask the user to
        confirm before proceeding (AskUserQuestion or explicit restated confirmation), since this writes to the
        shared remote Claude Design project and neither this command nor Anthropic's own docs can state what
        happens if it's re-run. Once confirmed, run `/design-sync` inside the chosen worktree (the first and
        only time this command invokes it), bring that worktree's resulting changes into the current checkout
        (merge or cherry-pick, whichever the user's actual repo workflow expects), and leave the remaining N-1
        worktrees for the user to remove manually (`git worktree remove`) rather than auto-deleting candidates
        they may still want to inspect.</action>
      <tool>Bash, AskUserQuestion</tool>
      <output>Chosen candidate synced and applied; other worktrees left in place, reported with their
        paths</output>
    </step>
  </phase>
</workflow>

<decision_criteria>
  <factor name="availability" precedence="1">
    <unmet>`claude-design` MCP tools are absent from this session. Stop in prepare; do not create any
      worktree.</unmet>
  </factor>
  <factor name="isolation" precedence="2">
    <unmet>A candidate's worktree cannot be created cleanly (path collision, dirty HEAD). Stop before spawning
      that candidate's subprocess rather than running it against the wrong tree.</unmet>
  </factor>
  <factor name="cost_awareness" precedence="3">
    <unmet>N is large enough that spawning that many nested sessions is likely to surprise the user. Confirm
      with AskUserQuestion before isolating anything, per the important rule above.</unmet>
  </factor>
</decision_criteria>

<output>
  Follows output_contract in CLAUDE.md.

  Generate mode: the N-row comparison table from present, each candidate's worktree path (so the user can
    inspect further or call this command again in apply mode), and the standing caveat that concurrent
    subprocess nesting is unverified at any N (per the important rule above — stated every run, not
    conditionally).

  Apply mode: which candidate was applied, the command used to bring it in, and the paths of the worktrees
    left behind for manual cleanup.
</output>
