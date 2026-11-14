#!/usr/bin/env python3
import importlib.util
import json
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
adapter = Path(sys.argv[1]).resolve()
spec = importlib.util.spec_from_file_location("search_guard", adapter)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
checked = 0


def check(condition, label):
    global checked
    checked += 1
    if not condition:
        raise AssertionError(label)


# Pattern positions can occur before or after options; all other operands must survive.
for command, expected in [
    ('rg "process.env"', ['rg', 'GUARD_SEARCH_PATTERN']),
    ('grep -rn "import.meta.env" .', ['grep', '-rn', 'GUARD_SEARCH_PATTERN', '.']),
    ('rg process.env "prod.env"', ['rg', 'GUARD_SEARCH_PATTERN', 'prod.env']),
    ('rg -- process.env prod.env', ['rg', '--', 'GUARD_SEARCH_PATTERN', 'prod.env']),
    ('rg -e process.env -e app.env prod.env', ['rg', '-e', 'GUARD_SEARCH_PATTERN', '-e', 'GUARD_SEARCH_PATTERN', 'prod.env']),
    ('grep -ineprocess.env prod.env', ['grep', '-ineGUARD_SEARCH_PATTERN', 'prod.env']),
    ('rg --regexp=process.env prod.env', ['rg', '--regexp=GUARD_SEARCH_PATTERN', 'prod.env']),
    ('grep --regexp process.env prod.env', ['grep', '--regexp', 'GUARD_SEARCH_PATTERN', 'prod.env']),
    ('rg -g src -e process.env prod.env', ['rg', '-g', 'src', '-e', 'GUARD_SEARCH_PATTERN', 'prod.env']),
    ('rg --color process.env prod.env', ['rg', '--color', 'process.env', 'GUARD_SEARCH_PATTERN']),
    ('rg prod.env -e process.env', ['rg', 'prod.env', '-e', 'GUARD_SEARCH_PATTERN']),
    ('rg -f patterns -e process.env prod.env', ['rg', '-f', 'patterns', '-e', 'GUARD_SEARCH_PATTERN', 'prod.env']),
    ('rg process.env>prod.env', ['rg', 'GUARD_SEARCH_PATTERN', '>', 'prod.env']),
    ('grep process.env <prod.env', ['grep', 'GUARD_SEARCH_PATTERN', '<', 'prod.env']),
    ('rg process.env >>prod.env', ['rg', 'GUARD_SEARCH_PATTERN', '>>', 'prod.env']),
    ('rg "process.env">out', ['rg', 'GUARD_SEARCH_PATTERN', '>', 'out']),
    ('grep --context=2 process.env prod.env', ['grep', '--context=2', 'GUARD_SEARCH_PATTERN', 'prod.env']),
]:
    check(shlex.split(module.search_command(command)) == expected, command)

for command in [
    'rg -f prod.env process.env', 'grep --file=prod.env process.env',
    'rg --unknown process.env prod.env',
    'rg -e', 'rg --regexp', 'rg -Q process.env', 'rg -D x process.env',
    'rg "process.env', 'rg "$PATTERN" prod.env', 'rg $(cat prod.env)',
    'rg `cat prod.env`', 'env X=1 rg process.env', 'bash -c "rg process.env"',
    'rg process.env | cat prod.env', 'rg process.env; cat prod.env',
    'rg process.env && cat prod.env', 'rg process.env\ncat prod.env',
    'rg process.env <<prod.env', 'cat prod.env', 'source config.env',
    'rg .env docs', 'rg .env.local docs', 'rg id_ed25519 docs',
    'rg "process.env.*" docs', 'rg "(process.env)" docs',
    # Quoted or escaped redirect characters are arguments, so prod.env stays a file operand.
    "grep -v '>' README.md prod.env", "rg -v '>' README.md prod.env",
    'grep -v "<" README.md prod.env', r'grep -hv \> README.md prod.env',
    "rg -- '>' x prod.env", "grep -v '>>' README.md prod.env",
    r'grep -v \>\> README.md prod.env', "grep -v '<' README.md prod.env",
    "grep -v x'>' README.md prod.env", "rg -v '>' README.md 'prod.env'",
    # BSD grep takes an optional --context value, so the next word may be the pattern.
    'grep -Z --context AWS prod.env', 'grep --context AWS prod.env',
]:
    check(module.search_command(command) == command, 'unchanged: ' + command)

with tempfile.TemporaryDirectory(prefix="guard-search-test-") as temporary:
    stub = Path(temporary) / "guard.py"
    stub.write_text('import sys\nsys.stdout.buffer.write(sys.stdin.buffer.read())\n'
                    'print("stub diagnostic", file=sys.stderr)\nsys.exit(int(sys.argv[1]))\n')

    def run(raw, code=0):
        return subprocess.run([sys.executable, '-I', str(adapter), sys.executable, str(stub), str(code)],
                              input=raw, capture_output=True)

    for raw in [b'{', b'[]', b'{}{}', b'{"tool_name":"Bash","tool_input":{"command":7}}',
                b'\xff', b'[' * 2000 + b']' * 2000]:
        result = run(raw)
        check(result.returncode == 2 and not result.stdout and b'Credential guard input' in result.stderr,
              'invalid event refuses before guard')
    for tool in ['Read', 'Write', 'Edit']:
        raw = json.dumps({'tool_name': tool, 'tool_input': {'file_path': 'prod.env'}}).encode()
        result = run(raw)
        check(result.returncode == 0 and result.stdout == raw, tool + ' byte-preserving passthrough')
    raw = b'{ "tool_name":"Bash", "tool_input":{"command":"cat prod.env"} }'
    for code in [1, 7]:
        result = run(raw, code)
        check(result.returncode == 2 and result.stdout == raw
              and result.stderr == b'stub diagnostic\nCredential guard failed with exit %d.\n' % code,
              f'guard exit {code} becomes blocking exit 2 with stdout and stderr passthrough')
    raw = json.dumps({'tool_name': 'Bash', 'tool_input': {'command': 'rg process.env prod.env'},
                      'session_id': 'unchanged', 'cwd': '/work'}).encode()
    result = run(raw)
    forwarded = json.loads(result.stdout)
    check(forwarded['session_id'] == 'unchanged' and forwarded['cwd'] == '/work'
          and shlex.split(forwarded['tool_input']['command']) == ['rg', 'GUARD_SEARCH_PATTERN', 'prod.env'],
          'only the inspection command changes')
    result = subprocess.run([sys.executable, str(adapter), str(Path(temporary) / 'missing')],
                            input=b'{}', capture_output=True)
    check(result.returncode == 2 and not result.stdout and b'could not start' in result.stderr,
          'missing guard refuses')

print(f'guard-search-arguments: {checked} checks passed')
