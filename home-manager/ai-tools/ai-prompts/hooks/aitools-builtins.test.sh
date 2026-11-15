#!/bin/bash
set -euo pipefail
python3 - "$1" <<'PY'
import json
import os
from pathlib import Path
import re
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
    def check(tool, inp, expected, guidance='', mode='claude', outside=False, **extra):
        payload = dict(tool_name=tool, tool_input=inp, cwd=str(root), **extra)
        result = subprocess.run(['bash', hook, mode], input=json.dumps(payload), env=env,
                                capture_output=True, text=True)
        if expected == 'block' and mode == 'codex':
            assert result.returncode == 2 and result.stderr.strip() and not result.stdout, (payload, result)
            reason = result.stderr
        else:
            assert result.returncode == 0 and not result.stderr, (payload, result)
            if expected == 'pass_':
                assert not result.stdout, (payload, result.stdout)
            else:
                out = json.loads(result.stdout)['hookSpecificOutput']
                assert out['permissionDecision'] == 'deny', (payload, out)
                reason = out['permissionDecisionReason']
        if expected == 'block':
            assert guidance and guidance in reason, (payload, reason)
            subcommand = ('apply' if mode == 'codex' else
                          dict(Read='read', Grep='search', Glob='find', Write='write', Edit='edit', MultiEdit='edit')[tool])
            assert re.search(r'aitools (?:--root (?:\S+|\x27[^\x27]*\x27) )?' + subcommand + r' ', reason), (payload, reason)
            assert ('--root' in reason) == outside, (payload, reason)
            assert ('aitools mktemp' in reason) == outside, (payload, reason)
            if outside:
                assert reason.index('--root') < reason.index('aitools mktemp'), reason
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
    check('Write', dict(file_path='/tmp/outside', content='x'), 'block',
          'aitools --root /tmp write /tmp/outside --stdin', outside=True)
    check('Write', dict(file_path='inside', content='x'), 'block', 'aitools write inside --stdin')
    check('Read', dict(file_path='/tmp/outside'), 'block', 'aitools read /tmp/outside')
    check('Edit', dict(file_path='/tmp/outside', old_string='a', new_string='b'), 'block',
          'aitools --root /tmp write /tmp/outside --stdin', outside=True)
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
    (root / 'CLAUDE.md').write_text('existing project instructions')
    check('Edit', dict(file_path='CLAUDE.md'), 'pass_')
    check('Edit', dict(file_path='nested/CLAUDE.md'), 'block', 'aitools edit')
    check('Read', dict(file_path='README.md'), 'block', 'aitools read README.md')
    check('Write', dict(file_path='nested/CLAUDE.md'), 'block', 'aitools write')
    check('Read', dict(file_path=str(config / 'skills/name/references/x.md')), 'pass_')
    check('Write', dict(file_path=str(config / 'skills/name/references/x.md')), 'block', 'aitools write')
    for relative in ('projects/project/session/tool-results/result.txt',
                     'plugins/cache/vendor/plugin/version/skills/name/SKILL.md',
                     'plugins/cache/vendor/plugin/version/skills/name/references/example.md'):
        check('Read', dict(file_path=str(config / relative)), 'pass_')
        check('Write', dict(file_path=str(config / relative)), 'block', 'aitools write')
    for relative in ('projects/project/tool-results/result.txt',
                     'projects/project/session/other/result.txt',
                     'projects/project/session/tool-results/../private.txt',
                     'plugins/cache/vendor/plugin/version/private.txt',
                     'plugins/cache/vendor/plugin/version/skills/../private.txt',
                     'plugins/other/skills/name/SKILL.md'):
        check('Read', dict(file_path=str(config / relative)), 'block', 'aitools read')
    task = f'/private/tmp/claude-{os.getuid()}/project/session/tasks/task.output'
    check('Read', dict(file_path=task), 'pass_')
    check('Read', dict(file_path=task.replace('/private/tmp/', '/tmp/')), 'pass_')
    check('Write', dict(file_path=task), 'block', f'aitools --root {Path(task).parent} write {task} --stdin', outside=True)
    for path in (task.replace(f'claude-{os.getuid()}', f'claude-{os.getuid() + 1}'),
                 task.replace('/tasks/', '/other/'), task + '.txt',
                 task.replace('/tasks/task.output', '/tasks/../private.output')):
        check('Read', dict(file_path=path), 'block', 'aitools read')
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
    outside_plan = root.parent / 'outside/p.md'
    check('Write', dict(file_path='../outside/p.md'), 'block',
          f'aitools --root {outside_plan.parent} write {outside_plan} --stdin', outside=True, permission_mode='plan')
    for tool in ('apply_patch', 'Edit', 'Write'):
        check(tool, dict(patch='test'), 'block', 'aitools apply --stdin', mode='codex')
    for tool in ('Edit', 'Write'):
        check(tool, dict(file_path='inside'), 'block', 'aitools apply --stdin', mode='codex')
        check(tool, dict(file_path='/tmp/outside'), 'block',
              'aitools --root /tmp write /tmp/outside --stdin', mode='codex', outside=True)
    for key in ('input', 'patch'):
        check('apply_patch', {key: '*** Begin Patch\n*** Add File: inside\n+x\n*** End Patch'},
              'block', 'aitools apply --stdin', mode='codex')
        check('apply_patch', {key: '*** Begin Patch\n*** Add File: /tmp/outside\n+x\n*** End Patch'},
              'block', 'aitools --root /tmp write /tmp/outside --stdin', mode='codex', outside=True)
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
