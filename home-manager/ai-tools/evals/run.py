#!/usr/bin/env python3
"""Eval runner for the AI config (Claude Code variants old/new, Codex sandbox variants dfa/ww).

Usage (run with `python3 -I`):
  run.py [--runtime claude|codex] [--variant old|new|dfa|ww] [--scenario ID ...] [--theme T ...]
         [--skip-managed] [--dry-run] [--run-id ID] [--scenarios-dir DIR]
         [--claude-model M] [--claude-effort E]
  run.py check RUN_DIR [--scenarios-dir DIR]

--runtime, --variant, --scenario and --theme may be repeated. --dry-run builds each fixture, prints
the command it would run, removes the fixture, and calls no model. `check` re-judges the transcripts
saved under RUN_DIR (~/.local/state/ai-evals/runs/<run-id>) without calling a model; file and cmd
checks need the fixture, which is kept only for runs that did not pass. Without it, a saved verdict
is reused only when the stored check hash equals the current check definition; otherwise the check
and the scenario are "unjudged" (neither pass nor fail) until the scenario is rerun.

The contract for scenarios, fixtures and checks is the T10 schema; scenarios live in scenarios/ and
fixture builders in fixtures/ next to this file (override with --scenarios-dir or
AI_EVALS_SCENARIOS_DIR; fixtures are then read from its sibling fixtures/ directory).
"""

import argparse
import hashlib
import json
import os
import re
import secrets
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
STATE = Path.home() / ".local/state/ai-evals"
RUNS = STATE / "runs"

THEMES = {
    "no-land", "unnecessary-question", "pr-body-quality", "unauthorized-merge",
    "over-verification", "rule-violation", "scope-too-narrow", "act-before-plan",
    "implausible-number", "invisible-progress", "hook-block", "aitools-edit", "codex-ab",
}
VARIANTS = {"claude": ["old", "new"], "codex": ["dfa", "ww"]}
CLAUDE_CONFIG = {"old": STATE / "config-old", "new": STATE / "config-new"}
FR003_TAGS = {"forbidden-bypass", "required-op"}
STATE_CHECK_TYPES = ("file_exists", "file_absent", "file_matches", "file_not_matches", "cmd")
# Review checks judge the meaning of the reply by regex. They are evaluated and reported as a
# non-binding hint; only the other (automatic) checks decide a scenario's status.
REVIEW_CHECK_TYPES = ("output_matches", "output_not_matches")
MANAGED_SETTINGS = Path("/Library/Application Support/ClaudeCode/managed-settings.json")
BUILTIN_EDIT_TOOLS = ("Edit", "Write", "MultiEdit", "NotebookEdit")
# Heuristic for a shell command that writes a file: in-place editors, tee, patch tools, Python file
# writes, and output redirection other than to /dev/null or another descriptor.
SHELL_WRITE_RE = re.compile(
    r"\bsed\s+(?:-\w+\s+)*-i|\bperl\s+-\w*i|\btee\b|\b(?:git\s+apply|patch)\s|\bwrite_(?:text|bytes)\("
    r"|\bopen\([^)]*,\s*['\"][wax]|(?<![\d&<>=-])>>?(?![&=>])\s*(?!/dev/null)[^\s|&;)]")

SCENARIO_FIELDS = {
    "id": str, "theme": str, "description": str, "runtimes": list,
    "requires_managed_settings": bool, "fixture": str, "fixture_args": list, "prompt": str,
    "max_turns": int, "timeout_sec": int, "checks": list,
}
OPTIONAL_SCENARIO_FIELDS = {"fr003": str}

# Required and optional fields per check type, with their types. A regex field may be null only
# where listed in NULLABLE.
CHECK_FIELDS = {
    "file_exists": ({"path": str}, {}),
    "file_absent": ({"path": str}, {}),
    "file_matches": ({"path": str, "regex": str}, {}),
    "file_not_matches": ({"path": str, "regex": str}, {}),
    "cmd": ({"run": str}, {"expect_exit": int}),
    "output_matches": ({"regex": str}, {}),
    "output_not_matches": ({"regex": str}, {}),
    "tool_called": ({"tool": str}, {"input_regex": str, "min": int}),
    "tool_not_called": ({"tool": str}, {"input_regex": str}),
    "tool_count_max": ({"tool": str, "max": int}, {"input_regex": str}),
    "hook_blocked": ({"regex": str}, {}),
    "exit_code": ({"expect": int}, {}),
}
NULLABLE = {"input_regex"}
REGEX_FIELDS = {"regex", "input_regex"}

FIXTURE_TIMEOUT = 300
CMD_CHECK_TIMEOUT = 120

# Messages the CLIs print when the config dir or CODEX_HOME holds no usable login.
CLAUDE_AUTH_RE = re.compile(
    r"not logged in|please run /login|invalid api key|oauth token (has )?expired|"
    r"authentication_error|invalid bearer token", re.I)
CODEX_AUTH_RE = re.compile(r"not logged in|please log in|codex login|401 unauthorized", re.I)
HOOK_MARKER_RE = re.compile(r"hook", re.I)


class ScenarioError(Exception):
    pass


# ---------------------------------------------------------------- scenarios

def scenarios_dir(arg):
    if arg:
        return Path(arg).resolve()
    env = os.environ.get("AI_EVALS_SCENARIOS_DIR")
    return Path(env).resolve() if env else HERE / "scenarios"


def validate_check(check, where):
    if not isinstance(check, dict):
        raise ScenarioError(f"{where}: check must be an object")
    ctype = check.get("type")
    if ctype not in CHECK_FIELDS:
        raise ScenarioError(f"{where}: unknown check type {ctype!r}")
    required, optional = CHECK_FIELDS[ctype]
    allowed = set(required) | set(optional) | {"type", "label"}
    extra = set(check) - allowed
    if extra:
        raise ScenarioError(f"{where} ({ctype}): unknown fields {sorted(extra)}")
    for name, typ in required.items():
        if name not in check:
            raise ScenarioError(f"{where} ({ctype}): missing field {name!r}")
    for name, typ in {**required, **optional}.items():
        if name not in check:
            continue
        value = check[name]
        if value is None and name in NULLABLE:
            continue
        if not isinstance(value, typ) or (typ is int and isinstance(value, bool)):
            raise ScenarioError(f"{where} ({ctype}): {name!r} must be {typ.__name__}")
        if name in REGEX_FIELDS:
            try:
                re.compile(value, re.M)
            except re.error as exc:
                raise ScenarioError(f"{where} ({ctype}): bad regex in {name!r}: {exc}") from None
    if "label" in check and not isinstance(check["label"], str):
        raise ScenarioError(f"{where}: label must be a string")
    if "path" in check:
        path = check["path"]
        if os.path.isabs(path) or ".." in Path(path).parts:
            raise ScenarioError(f"{where} ({ctype}): path must be relative to the fixture: {path!r}")
    for name in ("min", "max"):
        if name in check and check[name] < 0:
            raise ScenarioError(f"{where} ({ctype}): {name!r} must be >= 0")


def validate_scenario(data, path, fixtures):
    where = path.name
    if not isinstance(data, dict):
        raise ScenarioError(f"{where}: top level must be an object")
    extra = set(data) - set(SCENARIO_FIELDS) - set(OPTIONAL_SCENARIO_FIELDS)
    if extra:
        raise ScenarioError(f"{where}: unknown fields {sorted(extra)}")
    for name, typ in SCENARIO_FIELDS.items():
        if name not in data:
            raise ScenarioError(f"{where}: missing field {name!r}")
        value = data[name]
        if not isinstance(value, typ) or (typ is int and isinstance(value, bool)):
            raise ScenarioError(f"{where}: {name!r} must be {typ.__name__}")
    if data["id"] != path.stem:
        raise ScenarioError(f"{where}: id {data['id']!r} differs from the file name")
    if data["theme"] not in THEMES:
        raise ScenarioError(f"{where}: unknown theme {data['theme']!r}")
    if not data["runtimes"] or not set(data["runtimes"]) <= set(VARIANTS) \
            or len(set(data["runtimes"])) != len(data["runtimes"]):
        raise ScenarioError(f"{where}: runtimes must be a non-empty subset of {sorted(VARIANTS)}")
    if not all(isinstance(a, str) for a in data["fixture_args"]):
        raise ScenarioError(f"{where}: fixture_args must be strings")
    if not (fixtures / f"{data['fixture']}.sh").is_file():
        raise ScenarioError(f"{where}: fixture {data['fixture']!r} not found in {fixtures}")
    if not data["prompt"].strip():
        raise ScenarioError(f"{where}: empty prompt")
    if data["prompt"].startswith("-"):
        raise ScenarioError(f"{where}: prompt starting with '-' would be parsed as a CLI option")
    if data["max_turns"] <= 0 or data["timeout_sec"] <= 0:
        raise ScenarioError(f"{where}: max_turns and timeout_sec must be positive")
    if "fr003" in data:
        if data["fr003"] not in FR003_TAGS:
            raise ScenarioError(f"{where}: fr003 must be one of {sorted(FR003_TAGS)}")
        if "codex" not in data["runtimes"]:
            raise ScenarioError(f"{where}: fr003 needs the codex runtime")
    if not data["checks"]:
        raise ScenarioError(f"{where}: no checks")
    for i, check in enumerate(data["checks"]):
        validate_check(check, f"{where} checks[{i}]")


def load_scenarios(sdir):
    fixtures = sdir.parent / "fixtures"
    files = sorted(sdir.glob("*.json"))
    if not files:
        raise ScenarioError(f"no scenarios in {sdir}")
    scenarios, errors = [], []
    for path in files:
        try:
            data = json.loads(path.read_text(encoding="utf-8"))
            validate_scenario(data, path, fixtures)
            scenarios.append(data)
        except json.JSONDecodeError as exc:
            errors.append(f"{path.name}: invalid JSON: {exc}")
        except ScenarioError as exc:
            errors.append(str(exc))
    if errors:
        raise ScenarioError("invalid scenarios:\n  " + "\n  ".join(errors))
    return scenarios, fixtures


# ---------------------------------------------------------------- commands

def child_env(work):
    # Session variables of a Claude Code process that launched this runner (CLAUDECODE, session ids,
    # host-managed provider) would change how the child CLI authenticates and behaves. The managed
    # settings re-apply their own CLAUDE_* env block in the child.
    env = {k: v for k, v in os.environ.items() if not k.startswith("CLAUDE")}
    env["PATH"] = f"{work / 'bin'}{os.pathsep}{env.get('PATH', '')}"
    return env


def build_command(scenario, runtime, variant, fixture, work, args):
    prompt = scenario["prompt"]
    if runtime == "claude":
        cmd = ["claude", "-p", prompt, "--output-format", "stream-json", "--verbose",
               "--max-turns", str(scenario["max_turns"]), "--no-session-persistence",
               "--strict-mcp-config"]
        if args.claude_model:
            cmd += ["--model", args.claude_model]
        if args.claude_effort:
            cmd += ["--effort", args.claude_effort]
        return cmd, {"CLAUDE_CONFIG_DIR": str(CLAUDE_CONFIG[variant])}
    # The installed codex wrapper passes -c sandbox_mode="danger-full-access" before "$@"; a later -c
    # for the same key wins, so both variants set it explicitly.
    if variant == "dfa":
        sandbox = ["-c", 'sandbox_mode="danger-full-access"']
    else:
        roots = [str(work), str(Path.home() / ".cache/nix")]
        sandbox = ["-c", 'sandbox_mode="workspace-write"',
                   "-c", "sandbox_workspace_write.network_access=true",
                   "-c", f"sandbox_workspace_write.writable_roots={json.dumps(roots)}"]
    # Codex 0.167.1 persists trust for a cwd with no trust level when the sandbox can write it
    # (app-server thread_processor.rs: `config.active_project.trust_level.is_none()` guards
    # set_project_trust_level), which appended fixture paths to the user's config.toml. Declaring the
    # fixture trusted for this invocation only makes that guard false; -c is not written back. The
    # key is the physical path because that is the key Codex wrote. An inline table is used because
    # Codex splits a dotted -c key on every '.'.
    trust = ["-c", f"projects={{ {json.dumps(str(fixture.resolve()))} = {{ trust_level = \"trusted\" }} }}"]
    # --ephemeral keeps the session files out of the user's CODEX_HOME.
    return (["codex", "exec", "--json", "--ephemeral", "--skip-git-repo-check", "-C", str(fixture), *trust,
             *sandbox, prompt], {})


def kill_group(proc):
    try:
        os.killpg(proc.pid, signal.SIGTERM)
        proc.wait(timeout=10)
    except subprocess.TimeoutExpired:
        os.killpg(proc.pid, signal.SIGKILL)
        proc.wait()
    except ProcessLookupError:
        pass


def run_cli(cmd, env, cwd, out_dir, timeout):
    """Runs the CLI in its own process group so a timeout also stops its children."""
    started = time.time()
    with open(out_dir / "transcript.jsonl", "wb") as out, open(out_dir / "stderr.txt", "wb") as err:
        proc = subprocess.Popen(cmd, cwd=cwd, env=env, stdin=subprocess.DEVNULL, stdout=out,
                                stderr=err, start_new_session=True)
        timed_out = False
        try:
            proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            timed_out = True
            kill_group(proc)
    return {"exit_code": proc.returncode, "timed_out": timed_out,
            "duration_sec": round(time.time() - started, 1)}


def make_work_dir():
    # A hex suffix keeps the realpath free of characters other than '/' and '.' that Claude Code would
    # rewrite in a project dir name, so claude_project_dir can derive that name exactly.
    while True:
        path = Path("/tmp") / f"ai-evals.{secrets.token_hex(4)}"
        try:
            path.mkdir(mode=0o700)
        except FileExistsError:
            continue
        return path.resolve()


def build_fixture(scenario, fixtures, log_path):
    work = make_work_dir()
    fixture = work / "repo"
    fixture.mkdir()
    builder = fixtures / f"{scenario['fixture']}.sh"
    env = {**child_env(work), "FIXTURE": str(fixture), "WORK": str(work)}
    with open(log_path, "wb") as log:
        try:
            proc = subprocess.run(["sh", str(builder), str(fixture), *scenario["fixture_args"]],
                                  cwd=work, env=env, stdin=subprocess.DEVNULL, stdout=log,
                                  stderr=subprocess.STDOUT, timeout=FIXTURE_TIMEOUT)
            code = proc.returncode
        except subprocess.TimeoutExpired:
            code = "timeout"
    return work, fixture, code


PREFLIGHT_FILE = ".ai-evals-preflight"


def aitools_preflight(fixture, env):
    """Checks that aitools accepts writes with cwd = fixture; returns None or the failure detail."""
    steps = [(["aitools", "write", PREFLIGHT_FILE, "--stdin"], "preflight\n"),
             (["aitools", "rm", PREFLIGHT_FILE], None)]
    for cmd, stdin in steps:
        try:
            proc = subprocess.run(cmd, cwd=fixture, env=env, input=stdin, capture_output=True,
                                  text=True, timeout=60,
                                  stdin=None if stdin is not None else subprocess.DEVNULL)
        except (OSError, subprocess.TimeoutExpired) as exc:
            return f"{shlex.join(cmd)}: {exc}"
        if proc.returncode != 0:
            return f"{shlex.join(cmd)} exited {proc.returncode}: {clip(proc.stdout + proc.stderr)}"
    if (fixture / PREFLIGHT_FILE).exists():
        return f"{PREFLIGHT_FILE} still exists after aitools rm"
    return None


def claude_project_dir(fixture, config_dir):
    """Session dir Claude Code keys by this fixture's cwd under the CLAUDE_CONFIG_DIR this run passed."""
    return Path(config_dir) / "projects" / str(fixture.resolve()).replace("/", "-").replace(".", "-")


def remove_claude_project_dir(fixture, config_dir):
    # Only the run's own config dir is touched; the user's ~/.claude is never a cleanup target.
    path = claude_project_dir(fixture, config_dir)
    if path.is_dir() and not path.is_symlink():
        shutil.rmtree(path)
        return [str(path)]
    return []


def fixture_listing(fixture, limit=200):
    entries = []
    for root, dirs, files in os.walk(fixture):
        dirs[:] = sorted(d for d in dirs if d not in (".git", ".jj"))
        for name in sorted(files):
            entries.append(os.path.relpath(os.path.join(root, name), fixture))
            if len(entries) >= limit:
                return entries + ["..."]
    return entries


# ---------------------------------------------------------------- runtime config

def file_record(path):
    """Path, symlink-resolved target and sha256 of the target (null when it is not a file)."""
    real = Path(os.path.realpath(os.path.expanduser(str(path))))
    return {"path": str(path), "resolved": str(real),
            "sha256": hashlib.sha256(real.read_bytes()).hexdigest() if real.is_file() else None}


def managed_hook_targets():
    data = json.loads(MANAGED_SETTINGS.read_text(encoding="utf-8"))
    targets = []
    for event, groups in data.get("hooks", {}).items():
        for group in groups:
            for hook in group.get("hooks", []):
                if hook.get("type") != "command":
                    continue
                tokens = shlex.split(hook["command"])
                paths = [tokens[0]] + [tokens[i + 1] for i, t in enumerate(tokens[:-1]) if t == "--config"]
                targets += [{"event": event, "matcher": group.get("matcher"), **file_record(t)} for t in paths]
    return targets


def hook_digests(directory):
    return {p.name: file_record(p)["sha256"] for p in sorted(directory.iterdir())} if directory.is_dir() else {}


def runtime_config():
    """Identifies the managed settings, hooks and Codex config the CLIs load, captured at run start."""
    managed = file_record(MANAGED_SETTINGS)
    codex_home = Path(os.environ.get("CODEX_HOME") or Path.home() / ".codex")
    codex_exe = shutil.which("codex")
    live = hook_digests(Path.home() / ".claude/hooks")
    frozen = hook_digests(CLAUDE_CONFIG["old"] / "hooks")
    return {
        "managed_settings": managed,
        "managed_hook_targets": managed_hook_targets() if managed["sha256"] else [],
        "codex_home": str(codex_home),
        "codex_sources": [file_record(codex_home / n) for n in ("config.toml", "hooks.json", "AGENTS.md")]
                         + [file_record(r) for r in sorted((codex_home / "rules").glob("*.rules"))],
        "codex_executable": file_record(codex_exe) if codex_exe else None,
        # Variant old runs with the live ~/.claude/hooks (managed settings name them by absolute
        # path); this shows whether they still equal the frozen copy.
        "old_hooks": {"live": live, "frozen": frozen,
                      "mismatched": sorted(n for n in live.keys() | frozen.keys()
                                           if live.get(n) != frozen.get(n))},
    }


# ---------------------------------------------------------------- transcripts

def read_events(path):
    events, bad = [], 0
    if not path.exists():
        return events, bad
    for line_no, line in enumerate(path.read_text(encoding="utf-8", errors="replace").splitlines(), 1):
        line = line.strip()
        if not line:
            continue
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            bad += 1
            continue
        if isinstance(obj, dict):
            events.append((line_no, obj))
    return events, bad


def block_text(content):
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return "\n".join(block_text(c.get("text", c.get("content", "")) if isinstance(c, dict)
                                    else c) for c in content)
    return "" if content is None else json.dumps(content, ensure_ascii=False)


def tool_input_text(name, inp):
    if name in ("Bash", "shell") and isinstance(inp, dict) and isinstance(inp.get("command"), str):
        return inp["command"]
    return json.dumps(inp, ensure_ascii=False, sort_keys=True)


def parse_claude(events):
    calls, results, seen = [], [], set()
    final, last_text, error = None, None, None
    final_line = last_text_line = None
    for line_no, ev in events:
        etype = ev.get("type")
        parent = ev.get("parent_tool_use_id")
        content = (ev.get("message") or {}).get("content")
        if etype == "assistant" and isinstance(content, list):
            for block in content:
                if not isinstance(block, dict):
                    continue
                if block.get("type") == "tool_use":
                    # stream-json may emit one event per content block of the same message.
                    key = block.get("id") or json.dumps(block, sort_keys=True)
                    if key in seen:
                        continue
                    seen.add(key)
                    name, inp = block.get("name", ""), block.get("input", {})
                    calls.append({"tool": name, "input": inp, "text": tool_input_text(name, inp),
                                  "id": block.get("id"), "parent_tool_use_id": parent})
                elif block.get("type") == "text" and parent is None:
                    last_text, last_text_line = block.get("text", ""), line_no
        elif etype == "user" and isinstance(content, list):
            for block in content:
                if isinstance(block, dict) and block.get("type") == "tool_result":
                    results.append({"tool_use_id": block.get("tool_use_id"),
                                    "is_error": bool(block.get("is_error")),
                                    "text": block_text(block.get("content")),
                                    "parent_tool_use_id": parent})
        elif etype == "result":
            if isinstance(ev.get("result"), str):
                final, final_line = ev["result"], line_no
            if ev.get("is_error"):
                error = final or ev.get("subtype")
    failed_ids = {r["tool_use_id"] for r in results if r["is_error"]}
    for call in calls:
        call["ok"] = call["id"] not in failed_ids
    # Claude Code reports a PreToolUse hook that exits 2 as an error tool_result naming the hook.
    hook_denials = [r for r in results if r["is_error"] and HOOK_MARKER_RE.search(r["text"])]
    return {"calls": calls, "hook_denials": hook_denials, "final_text": final,
            "final_source": "result event" if final is not None else
            ("last assistant text (no result event)" if last_text is not None else "none"),
            "final_line": final_line, "fallback_text": last_text, "fallback_line": last_text_line,
            "cli_error": error}


def parse_codex(events):
    calls, hook_denials, errors = [], [], []
    final = final_line = None
    for line_no, ev in events:
        etype = ev.get("type")
        if etype in ("error", "turn.failed"):
            errors.append(block_text(ev.get("message") or ev.get("error")))
            continue
        if etype != "item.completed" or not isinstance(ev.get("item"), dict):
            continue
        item = ev["item"]
        itype = item.get("type", item.get("item_type"))
        if itype == "command_execution":
            inp = {"command": item.get("command", "")}
            calls.append({"tool": "shell", "input": inp, "text": tool_input_text("shell", inp),
                          "id": item.get("id"), "status": item.get("status"),
                          "exit_code": item.get("exit_code"),
                          "ok": item.get("status") == "completed" and item.get("exit_code") in (0, None)})
            output = item.get("aggregated_output") or ""
            if (item.get("status") in ("declined", "failed") or item.get("exit_code") not in (0, None)) \
                    and HOOK_MARKER_RE.search(output):
                hook_denials.append({"tool_use_id": item.get("id"), "text": output})
        elif itype == "file_change":
            inp = {"changes": item.get("changes", [])}
            calls.append({"tool": "apply_patch", "input": inp,
                          "text": tool_input_text("apply_patch", inp), "id": item.get("id"),
                          "status": item.get("status"), "ok": item.get("status") == "completed"})
        elif itype == "mcp_tool_call":
            inp = {k: item.get(k) for k in ("server", "tool", "arguments")}
            calls.append({"tool": "mcp", "input": inp, "text": tool_input_text("mcp", inp),
                          "id": item.get("id"), "status": item.get("status"),
                          "ok": item.get("status") == "completed"})
        elif itype == "agent_message":
            final, final_line = item.get("text", ""), line_no
    return {"calls": calls, "hook_denials": hook_denials, "final_text": final,
            "final_source": "last agent_message" if final is not None else "none",
            "final_line": final_line, "fallback_text": None, "fallback_line": None,
            "cli_error": "; ".join(e for e in errors if e) or None}


def parse_transcript(runtime, path):
    events, bad = read_events(path)
    parsed = (parse_claude if runtime == "claude" else parse_codex)(events)
    parsed["event_count"] = len(events)
    parsed["unparsable_lines"] = bad
    return parsed


AITOOLS_GLOBAL_VALUE_OPTIONS = ("--root", "--lock-timeout")
# aitools commands that change file contents (`aitools schema`); reads, moves and copies are not edits.
AITOOLS_EDIT_COMMANDS = {
    ("edit",), ("insert",), ("replace",), ("apply",), ("transform",), ("move-lines",), ("write",),
    ("doc", "set"), ("doc", "delete"), ("doc", "merge"), ("doc", "patch"), ("md", "replace"),
    ("table", "set"),
}
SHELLS = {"sh", "bash", "zsh", "dash"}
COMMAND_PREFIXES = {"env", "command", "exec", "nohup", "time"}
SHELL_PUNCTUATION = ";&|()<>\n"
HEREDOC_RE = re.compile(r"(?<!<)<<-?(?!<)\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1")
ASSIGNMENT_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*=")


def split_heredocs(script):
    """The script with heredoc bodies removed, and the bodies in the order of their operators.

    Only `<<` outside quotes starts a heredoc, so a quoted inner script (`zsh -lc "...<<'PY'..."`)
    stays one word and is split again when it is classified on its own.
    """
    lines, kept, bodies, quote, i = script.split("\n"), [], [], None, 0
    while i < len(lines):
        line = lines[i]
        kept.append(line)
        i += 1
        delimiters, j = [], 0
        while j < len(line):
            ch = line[j]
            if quote == "'":
                quote = None if ch == "'" else quote
            elif ch == "\\":
                j += 1
            elif quote == '"':
                quote = None if ch == '"' else quote
            elif ch in "'\"":
                quote = ch
            elif m := HEREDOC_RE.match(line, j):
                delimiters.append(m.group(2))
                j = m.end()
                continue
            j += 1
        for delimiter in delimiters:
            body = []
            while i < len(lines) and lines[i].strip() != delimiter:
                body.append(lines[i])
                i += 1
            i += 1
            bodies.append("\n".join(body))
    return "\n".join(kept), bodies


def shell_segments(script):
    """Simple commands of a shell script as (words, heredoc bodies); None when it does not tokenize."""
    text, bodies = split_heredocs(script)
    lexer = shlex.shlex(text, posix=True, punctuation_chars=SHELL_PUNCTUATION)
    lexer.whitespace = " \t\r"
    lexer.whitespace_split = True
    try:
        tokens = list(lexer)
    except ValueError:
        return None
    body_iter = iter(bodies)
    segments, words, attached = [], [], []
    for token in tokens:
        if set(token) <= set(";&|()\n"):
            segments.append((words, attached))
            words, attached = [], []
            continue
        if token.startswith("<<") and not token.startswith("<<<"):
            attached.append(next(body_iter, ""))
        words.append(token)
    segments.append((words, attached))
    return [s for s in segments if s[0]]


def command_words(words):
    i = 0
    while i < len(words) and (ASSIGNMENT_RE.match(words[i]) or words[i] in COMMAND_PREFIXES):
        i += 1
    return words[i:]


def is_aitools_edit(words):
    if not words or os.path.basename(words[0]) != "aitools":
        return False
    rest, i = words[1:], 0
    while i < len(rest) and rest[i].split("=", 1)[0] in AITOOLS_GLOBAL_VALUE_OPTIONS:
        i += 1 if "=" in rest[i] else 2
    command = tuple(rest[i:i + 2])
    return command[:1] in AITOOLS_EDIT_COMMANDS or command in AITOOLS_EDIT_COMMANDS


def segment_text(words, bodies):
    # Rejoin so SHELL_WRITE_RE sees `2>file` and `> file` as written; quoting is dropped on purpose,
    # the regex looks for operators and call shapes, not for exact arguments.
    out = []
    for word in words:
        if out and word[:1] in "<>" and out[-1].isdigit():
            out[-1] += word
        else:
            out.append(word)
    return " ".join(out + bodies)


def classify_shell(script):
    """(aitools edit, other shell write, parsed) for one shell command string."""
    segments = shell_segments(script)
    if segments is None:
        return False, False, False
    aitools = shell = False
    for words, bodies in segments:
        words = command_words(words)
        if len(words) >= 3 and os.path.basename(words[0]) in SHELLS \
                and words[1].startswith("-") and "c" in words[1]:
            inner = classify_shell(words[2])
            if not inner[2]:
                return inner
            aitools, shell = aitools or inner[0], shell or inner[1]
        elif is_aitools_edit(words):
            aitools = True
        elif SHELL_WRITE_RE.search(segment_text(words, bodies)):
            shell = True
    return aitools, shell, True


def edit_means(parsed):
    """Edit means used by successful tool calls: builtin, apply_patch, aitools, shell.

    One shell call can count as both aitools and shell when it holds both kinds of command. A
    shell call that does not tokenize is counted in unparsed_calls instead of being guessed.
    """
    by_kind, unparsed = {}, 0
    for call in parsed["calls"]:
        if not call["ok"]:
            continue
        tool, inp = call["tool"], call["input"]
        found = []
        if tool in BUILTIN_EDIT_TOOLS:
            found.append(("builtin", [inp.get("file_path") or inp.get("notebook_path")]))
        elif tool == "apply_patch":
            found.append(("apply_patch", [c.get("path") for c in inp.get("changes", [])]))
        elif tool in ("Bash", "shell"):
            aitools, shell, parsed_ok = classify_shell(call["text"])
            unparsed += not parsed_ok
            found += [(kind, [clip(call["text"], 160)]) for kind, hit in
                      (("aitools", aitools), ("shell", shell)) if hit]
        for kind, targets in found:
            entry = by_kind.setdefault(kind, {"calls": 0, "targets": []})
            entry["calls"] += 1
            entry["targets"] += [t for t in targets if t not in entry["targets"]][:20 - len(entry["targets"])]
    return {"kinds": sorted(by_kind), "by_kind": by_kind, "unparsed_calls": unparsed}


def edit_bucket(means):
    if means is None:
        return "unrecorded"
    kinds = set(means["kinds"])
    if not kinds:
        return "none"
    if kinds == {"aitools"}:
        return "aitools-only"
    if kinds <= {"builtin", "apply_patch"}:
        return "builtin/apply_patch-only"
    if kinds == {"shell"}:
        return "shell-only"
    return "mixed"


def auth_failure(runtime, parsed, stderr_text):
    pattern = CLAUDE_AUTH_RE if runtime == "claude" else CODEX_AUTH_RE
    if parsed["calls"]:
        return None
    for source in (parsed["cli_error"], parsed["final_text"], stderr_text):
        if source and pattern.search(source):
            return pattern.search(source).group(0)
    return None


# ---------------------------------------------------------------- checks

def clip(text, limit=400):
    text = text or ""
    return text if len(text) <= limit else text[:limit] + f"... [{len(text) - limit} more chars]"


def matching_calls(parsed, check):
    regex = check.get("input_regex")
    pattern = re.compile(regex, re.M) if regex else None
    return [c for c in parsed["calls"]
            if c["tool"] == check["tool"] and (pattern is None or pattern.search(c["text"]))]


def file_target(check, fixture):
    if fixture is None or not fixture.is_dir():
        return None
    return fixture / check["path"]


def evaluate(check, parsed, meta, fixture, work):
    ctype = check["type"]
    if ctype in STATE_CHECK_TYPES and (fixture is None or not fixture.is_dir()):
        return None, {"unavailable": "fixture directory no longer exists"}
    if ctype == "file_exists":
        target = file_target(check, fixture)
        return target.exists(), {"path": str(target), "exists": target.exists()}
    if ctype == "file_absent":
        target = file_target(check, fixture)
        return not target.exists(), {"path": str(target), "exists": target.exists()}
    if ctype in ("file_matches", "file_not_matches"):
        target = file_target(check, fixture)
        if not target.is_file():
            return False, {"path": str(target), "missing": True}
        text = target.read_text(encoding="utf-8", errors="replace")
        m = re.search(check["regex"], text, re.M)
        observed = {"path": str(target), "match": m.group(0)[:200] if m else None}
        return (m is not None) == (ctype == "file_matches"), observed
    if ctype == "cmd":
        expect = check.get("expect_exit", 0)
        env = {**os.environ, "FIXTURE": str(fixture), "WORK": str(work), "EVALS_DIR": str(HERE)}
        try:
            proc = subprocess.run(["sh", "-c", check["run"]], cwd=fixture, env=env,
                                  stdin=subprocess.DEVNULL, capture_output=True, text=True,
                                  timeout=CMD_CHECK_TIMEOUT)
        except subprocess.TimeoutExpired:
            return False, {"timeout_sec": CMD_CHECK_TIMEOUT}
        return proc.returncode == expect, {"exit": proc.returncode, "expect_exit": expect,
                                           "stdout": clip(proc.stdout), "stderr": clip(proc.stderr)}
    if ctype in ("output_matches", "output_not_matches"):
        text, line = parsed["final_text"], parsed["final_line"]
        if text is None:
            text, line = parsed["fallback_text"], parsed["fallback_line"]
        observed = {"source": parsed["final_source"], "transcript_line": line, "text": clip(text)}
        if text is None:
            return False, observed
        m = re.search(check["regex"], text, re.M)
        observed["match"] = m.group(0)[:200] if m else None
        return (m is not None) == (ctype == "output_matches"), observed
    if ctype in ("tool_called", "tool_not_called", "tool_count_max"):
        calls = matching_calls(parsed, check)
        observed = {"count": len(calls), "total_calls": len(parsed["calls"]),
                    "matches": [clip(c["text"], 300) for c in calls[:10]]}
        if ctype == "tool_called":
            minimum = check.get("min", 1)
            observed["min"] = minimum
            return len(calls) >= minimum, observed
        if ctype == "tool_not_called":
            return not calls, observed
        observed["max"] = check["max"]
        return len(calls) <= check["max"], observed
    if ctype == "hook_blocked":
        pattern = re.compile(check["regex"], re.M)
        hits = [d for d in parsed["hook_denials"] if pattern.search(d["text"])]
        return bool(hits), {"hook_denials": len(parsed["hook_denials"]),
                            "matched": [clip(d["text"], 300) for d in hits[:5]],
                            "unmatched": [clip(d["text"], 300) for d in parsed["hook_denials"][:5]
                                          if d not in hits]}
    if ctype == "exit_code":
        return meta.get("exit_code") == check["expect"], {"exit_code": meta.get("exit_code"),
                                                          "timed_out": meta.get("timed_out")}
    raise ScenarioError(f"unknown check type {ctype!r}")


def check_hash(check):
    return hashlib.sha256(json.dumps(check, sort_keys=True, ensure_ascii=False).encode("utf-8")).hexdigest()


def judge(scenario, meta, out_dir, previous=None):
    runtime = meta["runtime"]
    parsed = parse_transcript(runtime, out_dir / "transcript.jsonl")
    stderr_path = out_dir / "stderr.txt"
    stderr_text = stderr_path.read_text(encoding="utf-8", errors="replace") if stderr_path.exists() else ""
    work = Path(meta["work"]) if meta.get("work") else None
    fixture = Path(meta["fixture"]) if meta.get("fixture") else None
    prev_checks = {c["index"]: c for c in (previous or {}).get("checks", [])}

    results = []
    for i, check in enumerate(scenario["checks"]):
        digest = check_hash(check)
        passed, observed = evaluate(check, parsed, meta, fixture, work)
        entry = {"index": i, "type": check["type"], "label": check.get("label"), "check_hash": digest,
                 "class": "review" if check["type"] in REVIEW_CHECK_TYPES else "automatic",
                 "pass": passed, "observed": observed}
        if passed is None:
            # The fixture was removed after a pass; reuse the verdict observed while it existed, but
            # only for the identical check definition. Results saved without a hash never match.
            prev = prev_checks.get(i)
            if prev and prev.get("check_hash") == digest and prev.get("pass") is not None:
                entry["pass"] = prev["pass"]
                entry["observed"] = {**prev.get("observed", {}), "carried_over": True}
            else:
                entry["observed"] = {**observed, "unjudged": "no saved verdict for this check "
                                     "definition; rerun the scenario to judge it"}
        results.append(entry)

    auth = auth_failure(runtime, parsed, stderr_text)
    if auth:
        if runtime == "claude":
            hint = (f"CLAUDE_CONFIG_DIR={meta.get('config_dir')} has no usable login ({auth!r}); "
                    f"log in with `CLAUDE_CONFIG_DIR={meta.get('config_dir')} claude` then /login")
        else:
            hint = f"codex has no usable login ({auth!r}); run `codex login`"
        status, error = "error", hint
    elif meta["exit_code"] != 0 or meta["timed_out"]:
        # The run did not finish normally, so the artifacts may reflect a partial attempt; the
        # checks are kept for diagnosis but the run is neither a pass nor a fail.
        status = "incomplete"
        error = (f"CLI timed out after {scenario['timeout_sec']}s" if meta["timed_out"]
                 else f"CLI exited {meta['exit_code']}")
    else:
        automatic = [r for r in results if r["class"] == "automatic"]
        if not automatic:
            status = "review-only"
        # A check that failed decides the scenario even when another one is unjudged.
        elif any(r["pass"] is False for r in automatic):
            status = "fail"
        elif any(r["pass"] is None for r in automatic):
            status = "unjudged"
        else:
            status = "pass"
        error = None
    return {
        "scenario": scenario["id"], "theme": scenario["theme"], "fr003": scenario.get("fr003"),
        "runtime": runtime, "variant": meta["variant"], "status": status, "error": error,
        "exit_code": meta.get("exit_code"), "timed_out": meta.get("timed_out"),
        "transcript": {"events": parsed["event_count"], "unparsable_lines": parsed["unparsable_lines"],
                       "tool_calls": len(parsed["calls"]),
                       "subagent_tool_calls": sum(1 for c in parsed["calls"]
                                                  if c.get("parent_tool_use_id")),
                       "hook_denials": len(parsed["hook_denials"]),
                       "final_text_source": parsed["final_source"],
                       "cli_error": clip(parsed["cli_error"]) if parsed["cli_error"] else None},
        "edit_means": edit_means(parsed),
        "checks": results,
    }


# ---------------------------------------------------------------- summary

def write_json(path, data):
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def summarize(run_dir):
    results, needs_review = [], []
    for path in sorted(run_dir.glob("*/*/result.json")):
        r = json.loads(path.read_text(encoding="utf-8"))
        results.append(r)
        for c in r.get("checks", []):
            if c["type"] in REVIEW_CHECK_TYPES:
                needs_review.append({
                    "run": f"{r['scenario']}/{r['runtime']}/{r['variant']}", "index": c["index"],
                    "type": c["type"], "label": c.get("label"), "hint_pass": c["pass"],
                    "transcript": str(path.parent / "transcript.jsonl"),
                    "line": c.get("observed", {}).get("transcript_line")})

    def tally(rows):
        out = {"total": len(rows)}
        for status in ("pass", "fail", "review-only", "unjudged", "incomplete", "error", "deferred"):
            out[status] = sum(1 for r in rows if r["status"] == status)
        return out

    by_variant, by_theme = {}, {}
    for r in results:
        key = f"{r['runtime']}/{r['variant']}"
        by_variant.setdefault(key, []).append(r)
        by_theme.setdefault(r["theme"], {}).setdefault(key, []).append(r)

    fr003 = {}
    for variant in VARIANTS["codex"]:
        rows = [r for r in results if r["runtime"] == "codex" and r["variant"] == variant
                and r["status"] != "deferred"]
        ab = [r for r in rows if r["theme"] == "codex-ab"]
        required = [r for r in ab if r.get("fr003") == "required-op"]
        fr003[variant] = {
            "success": sum(1 for r in rows if r["status"] == "pass"),
            "judged": sum(1 for r in rows if r["status"] in ("pass", "fail")),
            "incomplete": sum(1 for r in rows if r["status"] == "incomplete"),
            # A bypass is the forbidden effect happening (a failed state check); a reply that
            # words the refusal unexpectedly still fails the scenario but is not a bypass. An
            # incomplete run that already produced the forbidden effect is a bypass too.
            "forbidden_bypass_failures": sorted(
                r["scenario"] for r in ab
                if r.get("fr003") == "forbidden-bypass" and r["status"] in ("fail", "incomplete")
                and any(c["pass"] is False and c["type"] in STATE_CHECK_TYPES
                        for c in r.get("checks", []))),
            "required_ops_pass": sum(1 for r in required if r["status"] == "pass"),
            "required_ops_total": len(required),
        }

    buckets = ("aitools-only", "builtin/apply_patch-only", "shell-only", "mixed", "none", "unrecorded")
    edit_split = {}
    for key, rows in sorted(by_theme.get("aitools-edit", {}).items()):
        passed = [edit_bucket(r.get("edit_means")) for r in rows if r["status"] == "pass"]
        edit_split[key] = {b: passed.count(b) for b in buckets}

    # Runtime configs recorded at run start; one run dir can hold several invocations.
    configs, old_mismatch = [], set()
    for meta_path in sorted(run_dir.glob("*/*/meta.json")):
        meta = json.loads(meta_path.read_text(encoding="utf-8"))
        config = meta.get("runtime_config")
        if config is None:
            continue
        if config not in configs:
            configs.append(config)
        if meta["runtime"] == "claude" and meta["variant"] == "old":
            old_mismatch.update(config["old_hooks"]["mismatched"])

    summary = {
        "run_dir": str(run_dir),
        "old_hooks_mismatch": sorted(old_mismatch),
        "runtime_configs": configs,
        "aitools_edit_passes_by_means": edit_split,
        "by_variant": {k: tally(v) for k, v in sorted(by_variant.items())},
        "by_theme": {t: {k: tally(v) for k, v in sorted(d.items())} for t, d in sorted(by_theme.items())},
        "fr003_codex": fr003,
        "failed": sorted(f"{r['scenario']}/{r['variant']}" for r in results if r["status"] == "fail"),
        "errors": [{"run": f"{r['scenario']}/{r['variant']}", "error": r.get("error")}
                   for r in results if r["status"] == "error"],
        "incomplete": [{"run": f"{r['scenario']}/{r['variant']}", "error": r.get("error")}
                       for r in results if r["status"] == "incomplete"],
        "deferred": sorted(f"{r['scenario']}/{r['variant']}" for r in results
                           if r["status"] == "deferred"),
        "unjudged": sorted(f"{r['scenario']}/{r['variant']}" for r in results
                           if r["status"] == "unjudged"),
        "review_only": sorted(f"{r['scenario']}/{r['runtime']}/{r['variant']}" for r in results
                              if r["status"] == "review-only"),
        "needs_review": needs_review,
    }
    write_json(run_dir / "summary.json", summary)

    lines = [f"# Eval summary: {run_dir.name}", ""]
    if old_mismatch:
        lines += [f"> **WARNING: variant old ran with live ~/.claude/hooks that differ from "
                  f"config-old/hooks: {', '.join(sorted(old_mismatch))}. Its results do not measure the "
                  f"frozen hooks.**", ""]
    lines += ["## By variant", "",
             "Status comes from automatic checks only (repo state, tool calls, exit code); reply-text "
             "checks are listed under Needs review.", "",
             "| variant | pass | fail | review-only | unjudged | incomplete | error | deferred | total |",
             "|---|---|---|---|---|---|---|---|---|"]
    for key, t in summary["by_variant"].items():
        lines.append(f"| {key} | {t['pass']} | {t['fail']} | {t['review-only']} | {t['unjudged']} "
                     f"| {t['incomplete']} | {t['error']} | {t['deferred']} | {t['total']} |")
    lines += ["", "## By theme (pass/total)", ""]
    keys = list(summary["by_variant"])
    lines += ["| theme | " + " | ".join(keys) + " |", "|---" * (len(keys) + 1) + "|"]
    for theme, d in summary["by_theme"].items():
        cells = [f"{d[k]['pass']}/{d[k]['total']}" if k in d else "-" for k in keys]
        lines.append(f"| {theme} | " + " | ".join(cells) + " |")
    lines += ["", "## FR-003 (codex dfa vs ww)", "",
              "| metric | dfa | ww |", "|---|---|---|",
              f"| success (pass/judged) | {fr003['dfa']['success']}/{fr003['dfa']['judged']} | "
              f"{fr003['ww']['success']}/{fr003['ww']['judged']} |",
              f"| incomplete (excluded from success) | {fr003['dfa']['incomplete']} | {fr003['ww']['incomplete']} |",
              f"| codex-ab forbidden-bypass failures | {len(fr003['dfa']['forbidden_bypass_failures'])} | "
              f"{len(fr003['ww']['forbidden_bypass_failures'])} |",
              f"| codex-ab required ops pass | {fr003['dfa']['required_ops_pass']}/"
              f"{fr003['dfa']['required_ops_total']} | {fr003['ww']['required_ops_pass']}/"
              f"{fr003['ww']['required_ops_total']} |"]
    for variant in VARIANTS["codex"]:
        if fr003[variant]["forbidden_bypass_failures"]:
            lines.append(f"\nForbidden-bypass failures ({variant}): "
                         + ", ".join(fr003[variant]["forbidden_bypass_failures"]))
    if edit_split:
        lines += ["", "## aitools-edit passes by edit means", "",
                  "| variant | " + " | ".join(buckets) + " |", "|---" * (len(buckets) + 1) + "|"]
        lines += [f"| {k} | " + " | ".join(str(d[b]) for b in buckets) + " |" for k, d in edit_split.items()]
    if configs:
        lines += ["", f"## Runtime config at run start ({len(configs)} distinct)", ""]
        for config in configs:
            records = ([config["managed_settings"]] + config["managed_hook_targets"]
                       + config["codex_sources"]
                       + ([config["codex_executable"]] if config["codex_executable"] else []))
            lines += [f"- {rec['resolved']}: {rec['sha256'] or 'missing'}" for rec in records] + [""]
    if summary["failed"]:
        lines += ["", "## Failed", ""] + [f"- {f}" for f in summary["failed"]]
    if summary["unjudged"]:
        lines += ["", "## Unjudged: check changed since the saved result, rerun", ""] + [
            f"- {u}" for u in summary["unjudged"]]
    if summary["incomplete"]:
        lines += ["", "## Incomplete: CLI exited non-zero or timed out, not counted as pass or fail", ""] + [
            f"- {e['run']}: {e['error']}" for e in summary["incomplete"]]
    if summary["review_only"]:
        lines += ["", "## Review-only: no automatic check, not counted as pass or fail", ""] + [
            f"- {u}" for u in summary["review_only"]]
    if needs_review:
        lines += ["", "## Needs review: reply meaning, regex verdict is a non-binding hint", "",
                  "| run | check | regex hint | transcript:line |", "|---|---|---|---|"]
        for n in needs_review:
            hint = "pass" if n["hint_pass"] else "fail"
            label = f"#{n['index']} {n['type']}" + (f" ({n['label']})" if n["label"] else "")
            lines.append(f"| {n['run']} | {label} | {hint} | {n['transcript']}:{n['line'] or '-'} |")
    if summary["errors"]:
        lines += ["", "## Errors", ""] + [f"- {e['run']}: {e['error']}" for e in summary["errors"]]
    if summary["deferred"]:
        lines += ["", "## Deferred: measure after switch", ""] + [f"- {d}" for d in summary["deferred"]]
    (run_dir / "summary.md").write_text("\n".join(lines) + "\n", encoding="utf-8")
    return summary


# ---------------------------------------------------------------- commands

def selected_pairs(scenarios, args):
    pairs = []
    for s in scenarios:
        if args.scenario and s["id"] not in args.scenario:
            continue
        if args.theme and s["theme"] not in args.theme:
            continue
        for runtime in s["runtimes"]:
            if args.runtime and runtime not in args.runtime:
                continue
            for variant in VARIANTS[runtime]:
                if args.variant and variant not in args.variant:
                    continue
                pairs.append((s, runtime, variant))
    return pairs


def run_one(scenario, runtime, variant, fixtures, args, run_dir, config):
    tag = f"{scenario['id']}/{runtime}/{variant}"
    out_dir = None if args.dry_run else run_dir / scenario["id"] / variant
    if out_dir:
        out_dir.mkdir(parents=True, exist_ok=True)
        write_json(out_dir / "scenario.json", scenario)

    if args.skip_managed and scenario["requires_managed_settings"]:
        print(f"[deferred] {tag}: requires managed settings; measure after switch")
        if out_dir:
            write_json(out_dir / "result.json", {
                "scenario": scenario["id"], "theme": scenario["theme"],
                "fr003": scenario.get("fr003"), "runtime": runtime, "variant": variant,
                "status": "deferred", "error": None, "checks": []})
        return "deferred"

    if out_dir:
        log_path = out_dir / "fixture.log"
    else:
        with tempfile.NamedTemporaryFile(prefix="ai-evals-fixture.", suffix=".log", dir="/tmp",
                                         delete=False) as tmp:
            log_path = Path(tmp.name)

    work, fixture, code = build_fixture(scenario, fixtures, log_path)
    cmd, extra_env = build_command(scenario, runtime, variant, fixture, work, args)
    preflight = aitools_preflight(fixture, child_env(work)) if code == 0 else "skipped"
    if args.dry_run:
        status = "ok" if code == 0 else f"FAILED ({code}), log kept at {log_path}"
        print(f"[dry-run] {tag}\n  fixture: {fixture} builder exit: {status}\n  cwd: {fixture}")
        print(f"  aitools preflight: {preflight or 'ok'}")
        for k, v in extra_env.items():
            print(f"  env: {k}={v}")
        print(f"  env: PATH={work / 'bin'}:$PATH (CLAUDE* variables removed)")
        print(f"  timeout: {scenario['timeout_sec']}s\n  cmd: {shlex.join(cmd)}")
        ok = code == 0 and preflight is None
        if ok:
            log_path.unlink()
            shutil.rmtree(work)
        return "dry-run" if ok else "error"

    meta = {"scenario": scenario["id"], "runtime": runtime, "variant": variant,
            "work": str(work), "fixture": str(fixture), "command": cmd,
            "config_dir": extra_env.get("CLAUDE_CONFIG_DIR"), "fixture_exit": code,
            "aitools_preflight": preflight or "ok", "runtime_config": config}

    def finish(result):
        write_json(out_dir / "result.json", result)
        keep = result["status"] != "pass"
        meta["fixture_kept"] = keep
        meta["deleted_project_dirs"] = (remove_claude_project_dir(fixture, extra_env["CLAUDE_CONFIG_DIR"])
                                        if runtime == "claude" else [])
        if not keep:
            shutil.rmtree(work, ignore_errors=True)
        write_json(out_dir / "meta.json", meta)
        print(f"[{result['status']}] {tag}" + (f": {result['error']}" if result.get("error") else "")
              + (f" (fixture kept: {work})" if keep else ""))
        return result["status"]

    if code != 0:
        return finish({"scenario": scenario["id"], "theme": scenario["theme"],
                       "fr003": scenario.get("fr003"), "runtime": runtime, "variant": variant,
                       "status": "error", "error": f"fixture builder exited {code}; see {log_path}",
                       "checks": []})
    if runtime == "claude" and not CLAUDE_CONFIG[variant].is_dir():
        return finish({"scenario": scenario["id"], "theme": scenario["theme"],
                       "fr003": scenario.get("fr003"), "runtime": runtime, "variant": variant,
                       "status": "error",
                       "error": f"{CLAUDE_CONFIG[variant]} does not exist; run freeze-old-config.sh",
                       "checks": []})
    if preflight:
        return finish({"scenario": scenario["id"], "theme": scenario["theme"],
                       "fr003": scenario.get("fr003"), "runtime": runtime, "variant": variant,
                       "status": "error",
                       "error": f"harness-error: aitools cannot write in fixture ({preflight})",
                       "checks": []})

    env = {**child_env(work), **extra_env}
    try:
        meta.update(run_cli(cmd, env, fixture, out_dir, scenario["timeout_sec"]))
    except FileNotFoundError as exc:
        return finish({"scenario": scenario["id"], "theme": scenario["theme"],
                       "fr003": scenario.get("fr003"), "runtime": runtime, "variant": variant,
                       "status": "error", "error": f"cannot start {cmd[0]}: {exc}", "checks": []})
    meta["fixture_listing"] = fixture_listing(fixture)
    return finish(judge(scenario, meta, out_dir))


def cmd_run(argv):
    p = argparse.ArgumentParser(prog="run.py", description="Run AI config eval scenarios.")
    p.add_argument("--runtime", action="append", choices=sorted(VARIANTS))
    p.add_argument("--variant", action="append", choices=[v for vs in VARIANTS.values() for v in vs])
    p.add_argument("--scenario", action="append", nargs="+")
    p.add_argument("--theme", action="append", nargs="+")
    p.add_argument("--skip-managed", action="store_true")
    p.add_argument("--dry-run", action="store_true")
    p.add_argument("--run-id")
    p.add_argument("--scenarios-dir")
    p.add_argument("--claude-model", help="passed as --model to both Claude variants")
    p.add_argument("--claude-effort", help="passed as --effort to both Claude variants")
    args = p.parse_args(argv)
    args.scenario = [x for group in args.scenario or [] for x in group]
    args.theme = [x for group in args.theme or [] for x in group]
    unknown_themes = set(args.theme) - THEMES
    if unknown_themes:
        p.error(f"unknown theme(s): {sorted(unknown_themes)}")

    scenarios, fixtures = load_scenarios(scenarios_dir(args.scenarios_dir))
    unknown = set(args.scenario) - {s["id"] for s in scenarios}
    if unknown:
        p.error(f"unknown scenario(s): {sorted(unknown)}")
    pairs = selected_pairs(scenarios, args)
    if not pairs:
        p.error("no scenario/runtime/variant matches the filters")

    run_dir = None
    if not args.dry_run:
        run_id = args.run_id or time.strftime("%Y%m%dT%H%M%S")
        if "/" in run_id or run_id in (".", ".."):
            p.error("--run-id must be a plain name")
        run_dir = RUNS / run_id
        run_dir.mkdir(parents=True, exist_ok=True)

    config = None if args.dry_run else runtime_config()
    if config and config["old_hooks"]["mismatched"] and any(v == "old" for _, _, v in pairs):
        print("WARNING: ~/.claude/hooks differs from config-old/hooks ("
              + ", ".join(config["old_hooks"]["mismatched"]) + "); variant old will run the live hooks",
              file=sys.stderr)
    statuses = [run_one(s, rt, v, fixtures, args, run_dir, config) for s, rt, v in pairs]
    if args.dry_run:
        return 1 if "error" in statuses else 0
    summary = summarize(run_dir)
    print(f"summary: {run_dir / 'summary.md'}")
    for key, t in summary["by_variant"].items():
        print(f"  {key}: pass {t['pass']} fail {t['fail']} unjudged {t['unjudged']} "
              f"incomplete {t['incomplete']} error {t['error']} deferred {t['deferred']}")
    return 1 if any(st in ("fail", "incomplete", "error") for st in statuses) else 0


def cmd_check(argv):
    p = argparse.ArgumentParser(prog="run.py check", description="Re-judge saved transcripts.")
    p.add_argument("run_dir")
    p.add_argument("--scenarios-dir")
    args = p.parse_args(argv)
    run_dir = Path(args.run_dir).resolve()
    metas = sorted(run_dir.glob("*/*/meta.json"))
    if not metas:
        print(f"run.py check: no */*/meta.json under {run_dir}", file=sys.stderr)
        return 2
    sdir = scenarios_dir(args.scenarios_dir)
    statuses = []
    for meta_path in metas:
        out_dir = meta_path.parent
        meta = json.loads(meta_path.read_text(encoding="utf-8"))
        # Prefer the current scenario file so edited checks take effect; fall back to the copy saved
        # with the run.
        current = sdir / f"{meta['scenario']}.json"
        source = current if current.is_file() else out_dir / "scenario.json"
        scenario = json.loads(source.read_text(encoding="utf-8"))
        validate_scenario(scenario, Path(f"{meta['scenario']}.json"), sdir.parent / "fixtures")
        prev_path = out_dir / "result.json"
        previous = json.loads(prev_path.read_text(encoding="utf-8")) if prev_path.exists() else None
        if previous and previous.get("status") == "error" and "exit_code" not in meta:
            statuses.append("error")  # the CLI never ran; nothing to re-judge
            continue
        result = judge(scenario, meta, out_dir, previous)
        result["scenario_source"] = str(source)
        write_json(prev_path, result)
        statuses.append(result["status"])
        automatic = [c for c in result["checks"] if c["class"] == "automatic"]
        failed = [f"#{c['index']} {c['type']}" for c in automatic if c["pass"] is False]
        unjudged = [f"#{c['index']} {c['type']}" for c in automatic if c["pass"] is None]
        review = [f"#{c['index']} {'pass' if c['pass'] else 'fail'}" for c in result["checks"]
                  if c["class"] == "review"]
        print(f"[{result['status']}] {meta['scenario']}/{meta['runtime']}/{meta['variant']}"
              + (f" failed: {', '.join(failed)}" if failed else "")
              + (f" unjudged: {', '.join(unjudged)}" if unjudged else "")
              + (f" review hint: {', '.join(review)}" if review else "")
              + (f": {result['error']}" if result.get("error") else ""))
    summarize(run_dir)
    print(f"summary: {run_dir / 'summary.md'}")
    return 1 if any(st in ("fail", "unjudged", "incomplete", "error") for st in statuses) else 0


def main(argv):
    try:
        if argv[:1] == ["check"]:
            return cmd_check(argv[1:])
        return cmd_run(argv)
    except ScenarioError as exc:
        print(f"run.py: {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
