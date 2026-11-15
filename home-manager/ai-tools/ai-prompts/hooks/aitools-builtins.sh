#!/bin/bash
# File tools use aitools, except the measured Claude harness paths and non-text readers.
set -euo pipefail
command -v aitools >/dev/null 2>&1 || exit 0
command -v python3 >/dev/null 2>&1 || exit 0
input=$(cat)
python3 - "${1:-claude}" "$input" <<'PY'
import json
import os
from pathlib import Path
import shlex
import sys

mode, raw = sys.argv[1:]
try:
    payload = json.loads(raw)
except ValueError:
    sys.exit(0)
if not isinstance(payload, dict) or payload.get('hook_event_name', 'PreToolUse') != 'PreToolUse':
    sys.exit(0)
tool = payload.get('tool_name')
if mode == 'codex':
    if tool in ('apply_patch', 'Edit', 'Write'):
        print('Use aitools apply --stdin for a unified diff, or aitools edit PATH --stdin '
              'with {"old":"existing text","new":"replacement text"}. '
              'Outside the working directory use aitools --root <dir>; '
              'for temporary files use aitools mktemp.', file=sys.stderr)
        sys.exit(2)
    sys.exit(0)
if tool not in ('Read', 'Grep', 'Glob', 'Edit', 'Write', 'MultiEdit'):
    sys.exit(0)
inp = payload.get('tool_input')
if not isinstance(inp, dict):
    sys.exit(0)
cwd = payload.get('cwd') or os.getcwd()
if not isinstance(cwd, str):
    sys.exit(0)
cwd = os.path.abspath(cwd)
config = os.path.expanduser(os.environ.get('CLAUDE_CONFIG_DIR', '~/.claude'))
def absolute(path, base=cwd):
    return os.path.abspath(os.path.join(base, os.path.expanduser(path)))
def inside(path, directory):
    return os.path.commonpath((path, directory)) == directory
settings = {}
for file in (Path(config) / 'settings.json', Path(cwd) / '.claude/settings.json',
             Path(cwd) / '.claude/settings.local.json'):
    try:
        value = json.loads(file.read_text())
    except (OSError, ValueError):
        continue
    if isinstance(value, dict):
        settings.update(value)
path = inp.get('file_path', inp.get('path', cwd))
if not isinstance(path, str):
    sys.exit(0)
target = absolute(path)
if tool == 'Read' and Path(target).suffix.lower() in (
        '.png', '.jpg', '.jpeg', '.gif', '.webp', '.bmp', '.svg', '.avif', '.heic', '.pdf', '.ipynb'):
    sys.exit(0)
if tool == 'Read' and inside(target, absolute('skills', config)):
    sys.exit(0)
if tool in ('Read', 'Write', 'Edit', 'MultiEdit'):
    # Default memory paths are project-specific. Do not exempt the entire projects directory.
    projects = absolute('projects', config)
    relative = os.path.relpath(target, projects).split(os.sep)
    if len(relative) >= 3 and relative[0] != '..' and relative[1] == 'memory':
        sys.exit(0)
    custom_memory = settings.get('autoMemoryDirectory')
    if isinstance(custom_memory, str) and custom_memory and inside(target, absolute(custom_memory)):
        sys.exit(0)
if tool in ('Read', 'Write', 'Edit', 'MultiEdit'):
    if inside(target, absolute('plans', config)) and target.endswith('.md'):
        sys.exit(0)
    plans = settings.get('plansDirectory')
    if (payload.get('permission_mode') == 'plan' and isinstance(plans, str) and plans
            and inside(absolute(plans), cwd) and inside(target, absolute(plans))
            and target.endswith('.md')):
        sys.exit(0)
    if tool == 'Write' and target == os.path.join(cwd, 'CLAUDE.md'):
        sys.exit(0)

args = ['aitools']
if tool == 'Read':
    args += ['read', path]
    start = inp.get('offset', 1)
    limit = inp.get('limit')
    if isinstance(start, int) and not isinstance(start, bool) and start > 0:
        if isinstance(limit, int) and not isinstance(limit, bool) and limit > 0:
            args += ['--range', f'{start}:{start + limit - 1}']
        elif 'offset' in inp:
            args += ['--range', f'{start}:']
elif tool == 'Grep':
    args += ['search', str(inp.get('pattern', 'PATTERN')), path]
    if inp.get('-i'):
        args += ['--ignore-case']
    if inp.get('glob'):
        args += ['--glob', str(inp['glob'])]
elif tool == 'Glob':
    args += ['find', str(inp.get('pattern', '*')), path]
elif tool == 'Write':
    args += ['write', path, '--stdin']
else:
    args += ['edit', path, '--stdin']
reason = 'Use ' + shlex.join(args) + '.'
if tool in ('Edit', 'MultiEdit'):
    example = {'old': inp.get('old_string', 'existing text'),
               'new': inp.get('new_string', 'replacement text')}
    reason += ' JSON input: ' + json.dumps(example, ensure_ascii=False) + '.'
reason += ' Outside the working directory use aitools --root <dir>; for temporary files use aitools mktemp.'
print(json.dumps({'hookSpecificOutput': {'hookEventName': 'PreToolUse',
    'permissionDecision': 'deny', 'permissionDecisionReason': reason}}))
PY
