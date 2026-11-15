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
import re
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
inp = payload.get('tool_input')
cwd = payload.get('cwd') or os.getcwd()
if not isinstance(cwd, str):
    sys.exit(0)
cwd = os.path.abspath(cwd)
config = os.path.expanduser(os.environ.get('CLAUDE_CONFIG_DIR', '~/.claude'))
def absolute(path, base=cwd):
    return os.path.abspath(os.path.join(base, os.path.expanduser(path)))
def inside(path, directory):
    return os.path.commonpath((path, directory)) == directory
def outside_guidance(paths):
    for path in paths:
        target = absolute(path)
        if not inside(target, cwd):
            example = shlex.join(['aitools', '--root', os.path.dirname(target),
                                  'write', target, '--stdin'])
            return f' For a write outside the working directory, use {example}; for temporary files use aitools mktemp.'
    return ''
if mode == 'codex':
    if tool in ('apply_patch', 'Edit', 'Write'):
        paths = []
        if isinstance(inp, dict):
            paths = [inp[key] for key in ('file_path', 'path') if isinstance(inp.get(key), str)]
            patch = inp.get('input', inp.get('patch', ''))
        else:
            patch = inp
        if tool == 'apply_patch' and isinstance(patch, str):
            paths += re.findall(r'^\*\*\* (?:Add File|Update File|Delete File|Move to): (.+)$', patch, re.M)
        reason = ('Use aitools apply --stdin for a unified diff, or aitools edit PATH --stdin '
                  'with {"old":"existing text","new":"replacement text"}.')
        print(reason + outside_guidance(paths), file=sys.stderr)
        sys.exit(2)
    sys.exit(0)
if tool not in ('Read', 'Grep', 'Glob', 'Edit', 'Write', 'MultiEdit') or not isinstance(inp, dict):
    sys.exit(0)
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
if tool == 'Read':
    projects = absolute('projects', config)
    relative = os.path.relpath(target, projects).split(os.sep)
    if inside(target, projects) and len(relative) >= 4 and relative[2] == 'tool-results':
        sys.exit(0)
    cache = absolute('plugins/cache', config)
    relative = os.path.relpath(target, cache).split(os.sep)
    if inside(target, cache) and 'skills' in relative[:-1]:
        sys.exit(0)
    # Claude stores task output by project and session beneath the current user's temp root.
    for directory in (f'/private/tmp/claude-{os.getuid()}', f'/tmp/claude-{os.getuid()}'):
        relative = os.path.relpath(target, directory).split(os.sep)
        if (inside(target, directory) and len(relative) == 4 and relative[2] == 'tasks'
                and relative[3].endswith('.output')):
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
    if tool in ('Write', 'Edit') and target == os.path.join(cwd, 'CLAUDE.md'):
        sys.exit(0)

args = ['aitools']
if tool in ('Write', 'Edit', 'MultiEdit') and not inside(target, cwd):
    args += ['--root', os.path.dirname(target)]
    path = target
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
if tool in ('Write', 'Edit', 'MultiEdit'):
    reason += outside_guidance([path])
print(json.dumps({'hookSpecificOutput': {'hookEventName': 'PreToolUse',
    'permissionDecision': 'deny', 'permissionDecisionReason': reason}}))
PY
