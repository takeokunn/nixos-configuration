#!/usr/bin/env bash
# Acceptance gate for two codexSettings keys added to ./default.nix:
#   - codexSettings.hooks.PreToolUse — three shared guardrail scripts (already registered
#     under Claude Code, see ../claude-code/default.nix lines 135-153) wired as Codex-native
#     hooks via codexHookScript, each referenced by its Nix store path.
#   - codexSettings.profiles.fallback — a named Codex config profile giving a manual
#     `codex --profile fallback` escape hatch (model = "gpt-5.4-mini").
#
# Both are plain new keys in the codexSettings attrset. That attrset is auto-serialized: every
# key becomes a `-c key=value` CLI flag (codexSettingFlags, using the toTomlInline serializer)
# baked into the wrapped `codex` binary (codexWrapped, a symlinkJoin + makeWrapper derivation).
# codexWrapped and codexSettingFlags are internal names inside default.nix's `let` block — the
# module's returned attrset exposes only home.packages / xdg.configFile / home.sessionVariables
# — so the only way to see the literal flag string Codex will actually receive is to build the
# wrapper and read the script makeWrapper generated, not to `nix eval` the let-bindings from
# outside the file.
#
# This gate therefore has two parts, both checked against the real artifact rather than a
# hand-written expectation disconnected from it:
#
#   (A) nix build: construct codexWrapped exactly the way flake.nix's `ai-tools`
#       homeManagerModule constructs it for a real consumer (same overlay, same nurPkgs /
#       llmAgentsPkgs / mcp-servers-nix wiring, read from this repo's own flake inputs via
#       builtins.getFlake — not hand-rolled mocks), then grep the `-c` flags makeWrapper baked
#       into $out/bin/codex's exec line. This is a REAL BUILD, not an eval-only shortcut: it is
#       the only way to observe codexSettingFlags because that name is not exported. In local
#       testing — against a WARM Nix store, with llmAgentsPkgs.codex and the mcp-servers-nix
#       overlay's unfree google-chrome dependency (pulled in for the playwright MCP server)
#       already built and cached — this build completed in well under two minutes. That figure
#       is not guaranteed on a cold checkout (a fresh CI runner with no cached substitutes for
#       either of those): a cold build of the same derivation graph could legitimately take
#       much longer, and nothing here would distinguish "still building" from "hung" without a
#       bound, so the build below runs under `timeout`. This is not `nix build
#       .#darwinConfigurations.M4-Max...`, which would additionally evaluate and build the rest
#       of the system.
#
#   (B) hook behavior: extract the three hook scripts' store paths from the flags found in (A)
#       — so the exact binaries under test are the ones Nix wired in, never a path guessed from
#       source — and execute each directly with the PreToolUse stdin JSON shape codex-rs sends
#       (tool_name / tool_input.command, plus other fields these scripts do not read). This
#       reuses the black-box approach ../ai-prompts/verify-prompts.sh already uses for the same
#       three scripts under Claude Code's stdin shape (see its bh_run/bh_case_git, ~line 744).
#
# Usage:  ./verify-codex-hooks.sh
# Exit 0 only if every assertion below passed; non-zero otherwise, with the failing assertion
# named.

set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$here/../../.." && pwd)"

failures=0
checks=0

pass() {
  checks=$((checks + 1))
  printf '  ok   %s\n' "$1"
}
fail() {
  checks=$((checks + 1))
  failures=$((failures + 1))
  printf '  FAIL %s\n' "$1"
  if [ -n "${2:-}" ]; then
    printf '%s\n' "$2" | perl -pe 's/^/         /'
  fi
}

# Scratch dir for the build output and hook stderr capture. Lives under TMPDIR, never inside
# the repository — matches the convention ../ai-prompts/verify-prompts.sh uses for its own
# hook-behavior fixture (bh_tmp), for the same reason: a scratch dir inside the tree is one
# .gitignore edit away from being committed, and an interrupted run would otherwise leave a
# `result` symlink for the next `git status` to explain.
tmp=""
cleanup() {
  [ -n "${tmp:-}" ] && rm -rf "${tmp:?}" 2>/dev/null
}
trap cleanup EXIT INT TERM
tmp="$(mktemp -d "${TMPDIR:-/tmp}/verify-codex-hooks.XXXXXX")"

echo "== (A) building codexWrapped =="

# Reproduces flake.nix's flake.homeManagerModules.ai-tools construction (module args block,
# ~line 121-140): the mcp-servers-nix overlay on pkgs, nurPkgs from nur-packages evaluated
# against that pkgs, llmAgentsPkgs from llm-agents.packages.${system}, and allowUnfree=true
# (hosts/M4-Max/default.nix:24 and home-manager/advanced.nix:18/71 all set it — codex's own
# MCP wiring pulls in google-chrome for the playwright server, which is unfree). All of it
# read from this repo's own flake inputs via builtins.getFlake, not fabricated.
cat >"$tmp/build-expr.nix" <<NIXEOF
let
  flake = builtins.getFlake (toString $repo_root);
  inputs = flake.inputs;
  system = builtins.currentSystem;
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [ inputs.mcp-servers-nix.overlays.default ];
    config.allowUnfree = true;
  };
  nurPkgs = import inputs.nur-packages { inherit pkgs; };
  llmAgentsPkgs = inputs.llm-agents.packages.\${system};
  mcp-servers-nix = inputs.mcp-servers-nix;
  codexModule = import $repo_root/home-manager/ai-tools/codex/default.nix {
    inherit pkgs nurPkgs llmAgentsPkgs mcp-servers-nix;
  };
in
  builtins.elemAt codexModule.home.packages 0
NIXEOF

build_log="$tmp/build.log"
# 900s (15 min) is generous for a cold build of this small a derivation graph — a legitimately
# slow-but-succeeding cold build should still land well inside it — while still turning a truly
# hung/stuck build into a reported failure instead of an indefinitely silent command.
build_timeout_secs=900
if command -v nix >/dev/null 2>&1; then
  if timeout "${build_timeout_secs}s" nix build --impure --file "$tmp/build-expr.nix" -o "$tmp/result" >"$build_log" 2>&1; then
    pass "nix build of codexWrapped (real derivation, this repo's own flake inputs) succeeded"
  else
    build_status=$?
    if [ "$build_status" -eq 124 ]; then
      fail "nix build of codexWrapped timed out after ${build_timeout_secs}s" "$(tail -40 "$build_log")"
    else
      fail "nix build of codexWrapped failed" "$(tail -40 "$build_log")"
    fi
  fi
else
  fail "nix is not on PATH" "cannot build codexWrapped; every check below has nothing to run against"
fi

wrapper_path="$tmp/result/bin/codex"
if [ -x "$wrapper_path" ]; then
  pass '$out/bin/codex exists and is executable'
else
  fail '$out/bin/codex is missing or not executable' "checked: $wrapper_path"
fi

wrapper_content="$(cat "$wrapper_path" 2>/dev/null)"
wrapper_bytes="$(printf '%s' "$wrapper_content" | wc -c | tr -d ' ')"
if [ "${wrapper_bytes:-0}" -ge 200 ]; then
  pass "wrapper script is non-trivial ($wrapper_bytes bytes)"
else
  # An empty/missing wrapper is the vacuous-pass trap for section (B): every grep below would
  # find nothing and every "does not contain X" check would report FAIL correctly, but a
  # weaker check (e.g. checking only that the flags are *absent*) could pass on nothing at all.
  # Asserting substance up front keeps that failure mode from hiding inside a green run.
  fail "wrapper script looks empty or missing" "$wrapper_path is ${wrapper_bytes:-0} bytes"
fi

echo "== (B) parsing the -c flags makeWrapper actually baked in =="

# shared/default.nix's guardrailHookNames is the single source of truth for which hooks must be
# wired (claude-code/default.nix asserts against this exact list). This section used to restate
# the same three names as its own hardcoded literal, both for the count assertion below and for
# extracting each hook's store path. The two copies could drift silently: a guardrail hook added
# to guardrailHookNames — and fully wired into codex/default.nix, which maps over that same list —
# would leave this gate checking (and reporting green for) only the original three, forever. Parse
# the roster out of the shared file instead, and fail loudly on a broken parse rather than falling
# back to an empty roster that would pass every check below vacuously.
shared_nix="$here/../shared/default.nix"
guardrail_names="$(
  if [ -f "$shared_nix" ]; then
    perl -0777 -ne 'if (/guardrailHookNames\s*=\s*\[(.*?)\]/s) { my $body = $1; while ($body =~ /"([^"]+)"/g) { print "$1\n" } }' "$shared_nix"
  fi
)"
guardrail_count="$(printf '%s\n' "$guardrail_names" | grep -c .)"
if [ ! -f "$shared_nix" ]; then
  fail "could not read shared/default.nix to derive the guardrail hook roster" "expected at: $shared_nix"
elif [ "${guardrail_count:-0}" -eq 0 ]; then
  fail "parsed an empty guardrailHookNames list from shared/default.nix" \
    "the parser found no quoted entries inside guardrailHookNames = [ ... ] -- that is a broken
parse, not an empty roster, and must not silently degrade into a REQUIRED set of zero"
else
  pass "guardrail hook roster derived from shared/default.nix ($guardrail_count names: $(printf '%s' "$guardrail_names" | tr '\n' ' '))"
fi

# Each codexSettings key becomes one `-c 'key=value'` word in the exec line (see
# codexSettingFlags in default.nix). No value here contains a literal single quote — string
# leaves are JSON-double-quoted via toTomlInline's builtins.toJSON branch — so the first `'`
# after `-c '<name>=` is unambiguously the closing delimiter for that flag, and non-greedy
# matching finds it without needing full shell-quote parsing.
hooks_flag="$(printf '%s' "$wrapper_content" | perl -ne "print \$1 if /-c '(hooks=.*?)'/")"
profiles_flag="$(printf '%s' "$wrapper_content" | perl -ne "print \$1 if /-c '(profiles=.*?)'/")"

if [ -n "$hooks_flag" ]; then
  pass "a hooks=... -c flag is present in the wrapper"
else
  fail "no hooks=... -c flag found in the wrapper" "$(printf '%s' "$wrapper_content" | head -c 400)"
fi

case "$hooks_flag" in
*'matcher = "^Bash$"'*) pass 'hooks flag carries matcher = "^Bash$"' ;;
*) fail 'hooks flag does not carry matcher = "^Bash$"' "$hooks_flag" ;;
esac

# Extract each roster hook's store path from the flag text itself, so the scripts executed in
# section (C) are the exact ones Nix wired in — never a path re-derived from source, which would
# test the test's own assumption about writeShellScript's naming rather than the wiring. This
# loop scales with the roster derived above: a name added to guardrailHookNames but never wired
# into codexSettings.hooks.PreToolUse is now caught generically, rather than staying invisible to
# a check that only ever knew about three hardcoded names.
git_hook_path=""
cd_hook_path=""
perl_hook_path=""
for name in $guardrail_names; do
  path="$(printf '%s' "$hooks_flag" | perl -ne "print \$1 if m{command = \"(/nix/store/[^\"]*-$name)\"}")"
  if [ -n "$path" ] && [ -x "$path" ]; then
    pass "hooks flag names an executable store path for $name"
  else
    fail "hooks flag does not name an executable store path for $name" "extracted: [$path]"
  fi
  case "$name" in
  block-destructive-git) git_hook_path="$path" ;;
  block-bare-cd) cd_hook_path="$path" ;;
  enforce-perl) perl_hook_path="$path" ;;
  esac
done

# The loop above is positive-only: it proves every *known-roster* name is present, but none of
# it would catch a script silently added to codexSettings.hooks.PreToolUse[0].hooks that is not in
# shared/default.nix's guardrailHookNames at all — most plausibly rtk-rewrite, which
# ../claude-code/default.nix deliberately excludes from Claude Code's own registration (rtk is not
# on PATH there, so every rewritten command would exit 127) and which a future edit could just as
# easily forget to exclude here. Close the world: assert the exact count of `command = "`
# occurrences in the flag against the roster size derived above, so an unexpected extra hook fails
# this check even if nobody ever names it explicitly, and a roster that grew in shared/default.nix
# without being fully wired here is caught by the same count mismatch.
hook_command_count="$(printf '%s' "$hooks_flag" | grep -o 'command = "' | wc -l | tr -d ' ')"
if [ "${guardrail_count:-0}" -gt 0 ] && [ "${hook_command_count:-0}" -eq "${guardrail_count:-0}" ]; then
  pass "hooks flag wires exactly $guardrail_count hook commands (closed-world, from shared/default.nix's guardrailHookNames)"
else
  fail "hooks flag wires ${hook_command_count:-0} hook commands, expected exactly ${guardrail_count:-0} (from shared/default.nix)" "$hooks_flag"
fi

if [ "$profiles_flag" = 'profiles={ fallback = { model = "gpt-5.4-mini" } }' ]; then
  pass 'profiles flag is exactly profiles={ fallback = { model = "gpt-5.4-mini" } }'
else
  fail "profiles flag does not match the expected fallback-model wiring" "got: [$profiles_flag]"
fi

echo "== (C) hook behavior, driven with codex-rs's PreToolUse stdin shape =="

# codex-rs sends more fields than these scripts read (session/call metadata, cwd, ...); each
# case below carries decoys so a pass proves the scripts pick tool_name/tool_input.command out
# of the real shape rather than out of a stripped-down fixture that happens to match.
#
# Unlike section (B), this section is NOT derived from the guardrailHookNames roster: each case
# encodes the specific stdin shape and expected block reason for one named hook, which cannot be
# generated generically for an arbitrary future name. A hook added to the roster gets structural
# coverage automatically from section (B) (wired, executable, counted); it gets no behavioral
# coverage here until a case is written for it by name, same as block-destructive-git,
# block-bare-cd and enforce-perl each were.
hb_run() {
  # $1 = script path, $2 = stdin json
  hb_out="$(printf '%s' "$2" | "$1" 2>"$tmp/stderr")"
  hb_status=$?
  hb_err="$(cat "$tmp/stderr" 2>/dev/null)"
}

# $1 label, $2 script path, $3 stdin json, $4 expected exit status, $5 substring stderr must
# contain when $4 is 2 (blocked). Exit 2 alone is ambiguous — bash itself exits 2 on a syntax
# error in the hook script — so a blocking case also has to show its reason.
hb_case() {
  hb_run "$2" "$3"
  if [ "$hb_status" != "$4" ]; then
    fail "$1: exit status $hb_status, expected $4" "stderr: $(printf '%s' "$hb_err" | head -3)"
  elif [ "$4" = "2" ] && [ -z "$hb_err" ]; then
    fail "$1: exited 2 as expected but stderr was empty" ""
  elif [ "$4" = "2" ] && [ -n "${5:-}" ]; then
    case "$hb_err" in
    *"$5"*) pass "$1 (exit 2, stderr mentions \"$5\")" ;;
    *) fail "$1: exited 2 but stderr does not mention \"$5\"" "$hb_err" ;;
    esac
  else
    pass "$1 (exit $4)"
  fi
}

if [ -n "$git_hook_path" ] && [ -x "$git_hook_path" ]; then
  hb_case "block-destructive-git blocks 'git reset --hard'" "$git_hook_path" \
    '{"session_id":"s1","call_id":"c1","cwd":"/tmp","tool_name":"Bash","tool_input":{"command":"git reset --hard"}}' \
    2 "irrecoverably"
  hb_case "block-destructive-git allows 'git status'" "$git_hook_path" \
    '{"session_id":"s1","call_id":"c2","cwd":"/tmp","tool_name":"Bash","tool_input":{"command":"git status"}}' \
    0
else
  fail "block-destructive-git behavior cases skipped" "no valid store path extracted in section (B)"
fi

if [ -n "$cd_hook_path" ] && [ -x "$cd_hook_path" ]; then
  hb_case "block-bare-cd blocks a bare 'cd /tmp'" "$cd_hook_path" \
    '{"session_id":"s1","call_id":"c3","cwd":"/tmp","tool_name":"Bash","tool_input":{"command":"cd /tmp"}}' \
    2 "no effect"
  hb_case "block-bare-cd allows 'cd /tmp && ls'" "$cd_hook_path" \
    '{"session_id":"s1","call_id":"c4","cwd":"/tmp","tool_name":"Bash","tool_input":{"command":"cd /tmp && ls"}}' \
    0
else
  fail "block-bare-cd behavior cases skipped" "no valid store path extracted in section (B)"
fi

if [ -n "$perl_hook_path" ] && [ -x "$perl_hook_path" ]; then
  hb_case "enforce-perl blocks 'sed -i s/a/b/ file'" "$perl_hook_path" \
    '{"session_id":"s1","call_id":"c5","cwd":"/tmp","tool_name":"Bash","tool_input":{"command":"sed -i s/a/b/ file"}}' \
    2 "perl"
  hb_case "enforce-perl allows perl -pi -e 's/a/b/' file" "$perl_hook_path" \
    '{"session_id":"s1","call_id":"c6","cwd":"/tmp","tool_name":"Bash","tool_input":{"command":"perl -pi -e '"'"'s/a/b/'"'"' file"}}' \
    0
else
  fail "enforce-perl behavior cases skipped" "no valid store path extracted in section (B)"
fi

echo
echo "== summary =="
echo "$checks checks, $failures failed"

if [ "$checks" -eq 0 ]; then
  echo "no checks ran — treat this as a failure, not a pass over nothing"
  exit 1
fi

exit $([ "$failures" -eq 0 ] && echo 0 || echo 1)
