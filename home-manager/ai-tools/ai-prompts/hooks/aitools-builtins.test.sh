#!/bin/bash
set -euo pipefail
python3 - "$1" <<'PY'
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

hook = str(Path(sys.argv[1]).resolve())
counts = dict(rewrite=0, block=0, pass_=0)
with tempfile.TemporaryDirectory(prefix='aitools-builtins-') as directory:
    root = Path(directory)
    config = root / '.claude'
    config.mkdir()
    stub = root / 'aitools'
    stub.write_text('#!/bin/sh\nexit 0\n')
    stub.chmod(0o755)
    env = dict(os.environ, HOME=str(root), CLAUDE_CONFIG_DIR=str(config),
               PATH=f'{root}:{os.environ["PATH"]}')
    def check(tool, inp, expected, guidance='', mode='claude', **extra):
        payload = dict(tool_name=tool, tool_input=inp, cwd=str(root), **extra)
        result = subprocess.run(['bash', hook, mode], input=json.dumps(payload), env=env,
                                capture_output=True, text=True)
        if expected == 'block' and mode == 'codex':
            assert result.returncode == 2 and result.stderr.strip() and not result.stdout, (payload, result)
            assert guidance in result.stderr, result.stderr
        else:
            assert result.returncode == 0 and not result.stderr, (payload, result)
            if expected == 'pass_':
                assert not result.stdout, (payload, result.stdout)
            else:
                out = json.loads(result.stdout)['hookSpecificOutput']
                assert out['permissionDecision'] == 'deny', (payload, out)
                assert guidance in out['permissionDecisionReason'], (payload, out)
        counts[expected] += 1

    check('Read', dict(file_path='a b.txt', offset=4, limit=3), 'block', "aitools read 'a b.txt' --range 4:6")
    check('Read', dict(file_path='x', limit=3), 'block', 'aitools read x --range 1:3')
    check('Read', dict(file_path='x', offset=4), 'block', 'aitools read x --range 4:')
    check('Read', dict(file_path='x'), 'block', 'aitools read x.')
    check('Grep', dict(pattern='a b', path='src', glob='*.py'), 'block', "aitools search 'a b' src --glob '*.py'")
    check('Glob', dict(pattern='*.nix', path='src'), 'block', "aitools find '*.nix' src")
    check('Edit', dict(file_path='x', old_string='before', new_string='after'), 'block',
          'JSON input: {"old": "before", "new": "after"}')
    check('MultiEdit', dict(file_path='x', edits=[]), 'block', 'aitools edit x --stdin')
    check('Write', dict(file_path='/tmp/outside', content='x'), 'block', '--root')
    for suffix in ('.png', '.JPG', '.jpeg', '.gif', '.webp', '.bmp', '.svg', '.avif', '.heic', '.pdf', '.ipynb'):
        check('Read', dict(file_path='asset' + suffix), 'pass_')
        check('Write', dict(file_path='asset' + suffix), 'block', 'aitools write')
    check('NotebookEdit', dict(notebook_path='a.ipynb'), 'pass_')
    check('Bash', dict(command='cat x'), 'pass_')
    check('Read', dict(file_path='x'), 'pass_', hook_event_name='PostToolUse')
    for tool in ('Read', 'Write', 'Edit', 'MultiEdit'):
        check(tool, dict(file_path=str(config / 'plans/p.md')), 'pass_')
    check('Write', dict(file_path=str(config / 'plans/../private.md')), 'block', 'aitools write')
    check('Write', dict(file_path='CLAUDE.md'), 'pass_')
    check('Edit', dict(file_path='CLAUDE.md'), 'block', 'aitools edit')
    check('Write', dict(file_path='nested/CLAUDE.md'), 'block', 'aitools write')
    check('Read', dict(file_path=str(config / 'skills/name/references/x.md')), 'pass_')
    check('Write', dict(file_path=str(config / 'skills/name/references/x.md')), 'block', 'aitools write')
    for tool in ('Read', 'Edit', 'Write', 'MultiEdit'):
        check(tool, dict(file_path=str(config / 'projects/project/memory/topic.md')), 'pass_')
    check('Write', dict(file_path=str(config / 'projects/project/notes.md')), 'block', 'aitools write')
    check('Read', dict(file_path=str(config / 'settings.json')), 'block', 'aitools read')
    (config / 'settings.json').write_text(json.dumps(dict(plansDirectory='plans', autoMemoryDirectory='mem')))
    for tool in ('Read', 'Write', 'Edit'):
        check(tool, dict(file_path='mem/topic.md'), 'pass_')
    check('Write', dict(file_path='plans/p.md'), 'pass_', permission_mode='plan')
    check('Read', dict(file_path='plans/p.md'), 'pass_', permission_mode='plan')
    check('Write', dict(file_path='plans/p.md'), 'block', 'aitools write')
    check('Write', dict(file_path='plans/../x.md'), 'block', 'aitools write', permission_mode='plan')
    (config / 'settings.json').write_text(json.dumps(dict(plansDirectory='../outside')))
    check('Write', dict(file_path='../outside/p.md'), 'block', '--root', permission_mode='plan')
    for tool in ('apply_patch', 'Edit', 'Write'):
        check(tool, dict(patch='test'), 'block', 'aitools apply --stdin', mode='codex')
    check('Read', dict(file_path='x'), 'pass_', mode='codex')

    bare = root / 'bare'
    bare.mkdir()
    for tool in ('cat', 'python3'):
        (bare / tool).symlink_to(shutil.which(tool))
    env['PATH'] = str(bare)
    for mode, tool in [('claude', 'Write'), ('codex', 'apply_patch')]:
        result = subprocess.run([shutil.which('bash'), hook, mode], env=env, capture_output=True,
            input=json.dumps(dict(tool_name=tool, tool_input=dict(file_path='x'))), text=True)
        assert result.returncode == 0 and not result.stdout and not result.stderr, result
        counts['pass_'] += 1
print('aitools-builtins:', counts, 'failures=0')
PY
