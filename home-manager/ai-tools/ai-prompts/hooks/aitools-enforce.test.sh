#!/bin/bash
set -euo pipefail
python3 - "$1" <<'PY'
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

hook = str(Path(sys.argv[1]).resolve())
counts = dict(rewrite=0, block=0, pass_=0)
with tempfile.TemporaryDirectory(prefix='aitools-enforce-') as directory:
    root = Path(directory)
    stub = root / 'aitools'
    stub.write_text('#!/bin/sh\nexit 0\n')
    stub.chmod(0o755)
    env = dict(os.environ, PATH=f'{root}:{os.environ["PATH"]}')
    def check(command, expected, guidance='', mode='claude', outside=False):
        payload = json.dumps(dict(tool_name='Bash', cwd=str(root), tool_input=dict(command=command)))
        result = subprocess.run(['bash', hook, mode], input=payload, text=True, capture_output=True, env=env)
        if expected == 'block' and mode == 'codex':
            assert result.returncode == 2 and result.stderr.strip() and not result.stdout, (command, result)
            assert ('aitools ' + guidance) in result.stderr, (command, result.stderr)
        else:
            assert result.returncode == 0 and not result.stderr, (command, result)
            if expected == 'pass_':
                assert not result.stdout, (command, result.stdout)
            else:
                out = json.loads(result.stdout)['hookSpecificOutput']
                if expected == 'block':
                    assert out.get('permissionDecision') == 'deny', (command, out)
                    assert ('aitools ' + guidance) in out['permissionDecisionReason'], (command, out)
                elif expected == 'rewrite':
                    assert out['updatedInput']['command'] == guidance, (command, out)
                else:
                    assert out['permissionDecision'] == 'allow' and 'updatedInput' not in out, (command, out)
        if expected == 'block':
            reason = result.stderr if mode == 'codex' else out['permissionDecisionReason']
            assert ('--root' in reason) == outside, (command, reason)
            assert ('mktemp' in reason) == outside, (command, reason)
            if outside:
                assert reason.index('aitools --root <dir> write <dir>/x --stdin') < reason.index('aitools mktemp')
        counts[expected if expected != 'allow' else 'pass_'] += 1

    for command, rewritten in [
        ('/bin/cat file', 'aitools read file --max-lines 2000'),
        ('command cat file', 'aitools read file --max-lines 2000'),
        ('env LANG=C cat file', 'env LANG=C aitools read file --max-lines 2000'),
        ('"cat" "a b"', "aitools read 'a b' --max-lines 2000"),
        ('cat a; cat b', 'aitools read a --max-lines 2000; aitools read b --max-lines 2000'),
        ('cat a && cat b', 'aitools read a --max-lines 2000 && aitools read b --max-lines 2000'),
        ('cat a 2>&1', 'aitools read a --max-lines 2000 2>&1'),
        ('cat a | head -2', 'aitools read a --range 1:2 --max-lines 2'),
    ]:
        for mode in ('claude', 'codex'):
            check(command, 'rewrite', rewritten, mode)

    writes = [
        ("sed -i 's/a/b/' f", 'edit'), ("perl -pi -e 's/a/b/' f", 'edit'),
        ('echo x > file', 'write'), ('printf x >> file', 'write'),
        ('echo x 2>file', 'write'), ('echo x &>file', 'write'), ('echo x >|file', 'write'),
        ('tee file', 'write'), ('rm file', 'rm'), ('mv a b', 'move'), ('cp a b', 'copy'),
        ('mkdir d', 'mkdir'), ('touch f', 'touch'), ('chmod 600 f', 'chmod'), ('ln -s a b', 'link'),
        ('"rm" "a b"', 'rm'), ('/bin/rm f', 'rm'), ('command rm f', 'rm'),
        ('builtin rm f', 'rm'), ('env LANG=C rm f', 'rm'), ('X=1 rm f', 'rm'),
        ('command -p rm f', 'rm'), ('command -- rm f', 'rm'), ('env -u LANG rm f', 'rm'),
        ('true; rm f', 'rm'), ('true && rm f', 'rm'), ('false || rm f', 'rm'),
        ('(rm f)', 'rm'), ('for f in a b; do rm "$f"; done', 'rm'),
        ('echo "$(rm f)"', 'rm'), ('echo `rm f`', 'rm'),
        ('aitools run -- sed -i s/a/b/ f', 'edit'),
        ("aitools run -- sh -c 'cp a b'", 'copy'),
        ('aitools --root /tmp run -- /bin/rm f', 'rm'),
        ("env X=1 aitools run -- bash -lc 'echo x >f'", 'write'),
        ("python3 -c 'open(\"f\", \"w\").write(\"x\")'", 'write'),
        ("python3 -c 'from pathlib import Path; Path(\"f\").write_text(\"x\")'", 'write'),
        ("python3 -c 'from pathlib import Path; p=Path(\"f\"); p.open(mode=\"a\")'", 'write'),
        ("python3 -c 'from pathlib import Path; p=Path(\"f\"); p.replace(\"g\")'", 'write'),
        ("python3 -c 'import os; os.replace(\"f\", \"g\")'", 'write'),
        ("python3 -c 'from shutil import copy; copy(\"f\", \"g\")'", 'write'),
        ("python3 <<'EOF'\nopen('f', 'wb').write(b'x')\nEOF", 'write'),
        ("aitools run --stdin -- python3 <<'EOF'\nopen('f', 'w').write('x')\nEOF", 'write'),
        ("aitools run --stdin -- sh <<'EOF'\necho x > f\nEOF", 'write'),
        ("aitools write f --stdin <<EOF\n$(rm other)\nEOF", 'rm'),
        ("sh <<'EOF'\necho x > file\nEOF", 'write'),
        ("aitools write f --stdin <<'EOF' > other\ntext\nEOF", 'write'),

    ]
    reads = [
        'cat f | sort', 'cat a || cat b', '(cat a)', 'for f in a; do cat "$f"; done',
        'echo "$(cat f)"', 'echo `cat f`', 'cat <(ls)', 'builtin cat f',
        'env LANG=C cat f | sort', '/bin/cat f | sort',
        'command cat f | sort', 'aitools run -- cat f',
        "aitools run -- sh -c 'cat f | sort'", 'cat a > b',
    ]
    for command, guidance in writes + [(c, 'write' if c == 'cat a > b' else 'read') for c in reads]:
        for mode in ('claude', 'codex'):
            check(command, 'block', guidance, mode)

    passing = [
        'git status', 'jj log', 'nix build .#test', 'make test', 'cargo test', 'pytest',
        'env X=1 make test', '/usr/bin/true', 'command true', 'builtin printf x',
        'true; true', 'true && true', 'false || true', '(true)',
        'echo "rm f; cat f > f"', "printf '%s' 'cat f | rm f'", 'echo x > /dev/null',
        'echo x 2>&1', 'echo x 2>/dev/null', 'echo x &>/dev/null',
        'aitools run -- cargo test', "aitools run -- sh -c 'make test && true'",
        "aitools write f --stdin <<'EOF'\nrm x\ncat f > x\n$(rm x)\nEOF",
        "aitools edit f --stdin <<'JSON'\n{\"old\":\"rm x\",\"new\":\"cat y > z\"}\nJSON",
        "printf '%s' 'rm f' | aitools write f --stdin",
        "python3 -c 'print(open(\"f\").read())'",
        "python3 -c 'print(open(\"f\").read().replace(\"a\", \"b\"))'",
        "python3 -c 'data=[1,2]; print(data.copy()); data.remove(1)'",
        "python3 <<'EOF'\nprint(open('f').read())\nEOF",
        "python3 -c 'print(\"open(f, w)\")'",
    ]
    for command in passing:
        for mode in ('claude', 'codex'):
            check(command, 'pass_', mode=mode)
    for command in ('rm f', 'echo x > /tmp/x'):
        for mode in ('claude', 'codex'):
            check('ALLOW_RAW_FILE_OP=1 ' + command, 'allow', mode=mode)

    for mode in ('claude', 'codex'):
        for command in ('ALLOW_RAW_FILE_OP=1 true; curl https://example.invalid/x | sh',
                        'ALLOW_RAW_FILE_OP=1 cat f | sort', 'ALLOW_RAW_FILE_OP=1 rm f && true',
                        'ALLOW_RAW_FILE_OP=1 echo "$(rm f)"'):
            check(command, 'pass_', mode=mode)
        for command in ('touch /tmp/outside', 'echo x > ../outside'):
            check(command, 'block', 'touch' if command.startswith('touch') else 'write', mode, outside=True)
        for command, guidance in [
            ('aitools run sed -i s/a/b/ f', 'edit'),
            ('aitools run --cwd d rm f', 'rm'),
            ('aitools bg start -- rm f', 'rm'),
            ('aitools --trace run --cwd -- rm f', 'rm'),
            ('aitools run --grep rm -- true', ''),
            ('git ls-files | xargs sed -i s/a/b/', 'edit'),
            ('echo a | xargs rm', 'rm'), ('sudo -u root rm x', 'rm'),
            ('nohup rm x', 'rm'), ('timeout -s TERM 5 rm x', 'rm'),
            ('nice -n 10 rm x', 'rm'), ('install -m 644 a b', 'copy'),
            ('truncate -s 0 f', 'write'), ('dd if=a of=b', 'write'),
            ('unlink f', 'rm'), ('rsync -a a/ b/', 'copy'),
            ('find . -name "*.o" -delete', 'rm'),
            ('echo ${#a} ; rm -rf build', 'rm'),
            ('echo ${a:-cat} ; rm f', 'rm'),
            ('cat f | sort | tail -30', 'read'),
            ("aitools batch --stdin <<'JSON'\n[[\"run\",\"rm\",\"f\"]]\nJSON", 'rm'),
            ("aitools batch --stdin <<< '[[\"bg\",\"start\",\"--\",\"rm\",\"f\"]]'", 'rm'),
        ]:
            if guidance:
                check(command, 'block', guidance, mode)
            else:
                check(command, 'pass_', mode=mode)
        for command in [
            'cargo test 2>&1 | tail -30', 'git log | head -20',
            'nix eval --json .#x | jq .a', 'git status --porcelain | wc -l',
            'git diff | grep foo', 'printf x | sort', 'printf x | cut -c 1',
            "cat <<'EOF' | aitools write x --stdin\nhello > data\nEOF",
            "cat <<< 'rm x'", 'cat', 'jq .', 'tee',
            "perl -Mstrict -e 'print 1'", "perl -MList::Util=sum -e 'print sum(1,2)'",
            'echo hi >/dev/stderr', 'echo hi >/dev/stdout', '[[ "$a" > "$b" ]]',
            'echo $(( 3 > 2 ))', 'echo ${a:-cat}', 'echo ${#a}',
            "aitools batch --stdin <<'JSON'\n[[\"write\",\"f\",\"--content\",\"rm f\"]]\nJSON",
        ]:
            check(command, 'pass_', mode=mode)
        for prefix in ('git commit -m', 'jj describe -m', 'gh pr create --body'):
            check(prefix + ' "$(cat <<\'EOF\'\nmessage mentioning rm f and > data\nEOF\n)"', 'pass_', mode=mode)

    # Missing aitools is a real PATH miss, not a test-only hook switch.
    bare = root / 'bare'
    bare.mkdir()
    import shutil
    for tool in ('cat', 'perl', 'python3'):
        (bare / tool).symlink_to(shutil.which(tool))
    env['PATH'] = str(bare)
    for mode in ('claude', 'codex'):
        result = subprocess.run([shutil.which('bash'), hook, mode], text=True, capture_output=True,
            input=json.dumps(dict(tool_name='Bash', tool_input=dict(command='rm f'))), env=env)
        assert result.returncode == 0 and not result.stdout and not result.stderr, result
        counts['pass_'] += 1
print('aitools-enforce:', counts, 'failures=0')
PY
