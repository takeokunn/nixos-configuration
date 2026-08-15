#!/usr/bin/env bash
# Acceptance gate for the ai-prompts tree (CLAUDE.md, agents/, commands/, hooks/).
#
# This repository is declarative configuration plus prose; there is no unit-test suite. The
# equivalent of a test here is: do the invariants that Nix cannot express still hold? Those
# invariants are checked below. Its sibling, ../agent-skills/verify-skills.sh, covers the
# skill collection; this one covers the prompts that consume it.
#
# Usage:  ./verify-prompts.sh
#
# The hook-behavior gate (10) runs the hooks in this tree. Point VERIFY_HOOKS_DIR at a
# directory of hooks extracted from another ref — `git show <ref>:path/to/hooks/x.sh` — to
# watch those cases fail against the unfixed versions. That is how a case in section (10) is
# shown to be a guard rather than a claim; the default is this directory's hooks/, never the
# installed copy under ~/.claude/hooks, which is a /nix/store symlink from the last switch.
#
# The confidentiality gate needs a denylist of tokens that must never reach this public
# repository. That list is itself confidential — it names clients and employers — so it
# lives outside the tree and is NOT committed. Point AGENT_SKILLS_DENYLIST at it, or place
# it at ~/.config/agent-skills/denylist, one token per line. If the file is absent the gate
# FAILS rather than passing: a confidentiality check that silently skips is worse than no
# check, because it still reads as green.

set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
agents_dir="$here/agents"
commands_dir="$here/commands"
ai_tools_dir="$here/.."
skills_dir="$ai_tools_dir/agent-skills/skills"
repo_root="$(cd "$here/../../.." && pwd)"
denylist_file="${AGENT_SKILLS_DENYLIST:-$HOME/.config/agent-skills/denylist}"

claude_md_max_lines="${CLAUDE_MD_MAX_LINES:-300}"

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
  return 0
}

echo "== corpus =="

# Every check below iterates the tree. If the tree were empty — a bad path, a rename, a
# partial checkout — each individual check would find nothing wrong and report ok. Assert up
# front that there is something to check, so the suite cannot pass over nothing.
md_count="$(find "$here" -name '*.md' -type f 2>/dev/null | wc -l | tr -d ' ')"
if [ "${md_count:-0}" -ge 20 ]; then
  pass "prompt corpus is populated ($md_count markdown files)"
else
  fail "prompt corpus looks empty or truncated" \
    "found ${md_count:-0} .md files under $here; expected at least 20"
fi

echo "== (1) inherits references are gone =="

# `inherits="skill#anchor"` was never resolved by anything at runtime: measured across the
# transcript corpus, the referenced skills were loaded zero times. The attribute read as
# composition while delivering nothing, so the redesign removes it entirely. Any survivor is
# a promise the corpus cannot keep.
inherits_hits="$(grep -rn 'inherits=' --include='*.md' "$here" 2>/dev/null)"
if [ -z "$inherits_hits" ]; then
  pass "no inherits= attributes remain"
else
  n="$(printf '%s\n' "$inherits_hits" | wc -l | tr -d ' ')"
  fail "inherits= still present in $n place(s)" \
    "$(printf '%s\n' "$inherits_hits" | head -20 | perl -pe "s{^\Q$here\E/}{}")"
fi

echo "== (2) command frontmatter shape =="

# codex/default.nix converts each command into a codex skill via shared.parseFrontmatter, which
# reads the description off the frontmatter line starting "description: " (found with
# shared.findLineWithPrefix, itself asserting a match) rather than a hardcoded line index. Its
# `assert` still makes a wrong shape a build failure — but only for the one file Nix happens to
# evaluate first, and only at build time. Check the whole set here.
shape_out="$(
  python3 - "$commands_dir" "$agents_dir" <<'PY'
import glob, os, sys

def lines_of(path):
    with open(path, encoding='utf-8', errors='replace') as fh:
        return fh.read().split('\n')

commands = sorted(glob.glob(os.path.join(sys.argv[1], '*.md')))
agents = sorted(glob.glob(os.path.join(sys.argv[2], '*.md')))
if not commands or not agents:
    raise SystemExit('found no command or agent files to check')

# (index, required prefix) per kind, mirroring the two converters in codex/default.nix.
spec = {
    'command': [(0, '---'), (2, 'description: '), (3, '---')],
    'agent': [(0, '---'), (1, 'name: '), (2, 'description: '), (3, '---')],
}
for kind, files in (('command', commands), ('agent', agents)):
    for f in files:
        got = lines_of(f)
        name = os.path.basename(f)
        if len(got) < 5:
            print(f'{kind} {name}: fewer than 5 lines')
            continue
        for idx, prefix in spec[kind]:
            if not got[idx].startswith(prefix):
                print(f'{kind} {name}: line {idx + 1} must start with {prefix!r}, '
                      f'got {got[idx][:40]!r}')
print(f'COUNT {len(commands)} commands, {len(agents)} agents')
PY
)"
shape_status=$?
if [ $shape_status -ne 0 ]; then
  fail "frontmatter shape check did not run" "$shape_out"
elif printf '%s' "$shape_out" | grep -qv '^COUNT '; then
  fail "frontmatter shape breaks the codex converter" \
    "$(printf '%s' "$shape_out" | grep -v '^COUNT ')"
else
  pass "every command and agent has the 4-line frontmatter codex expects ($(printf '%s' "$shape_out" | perl -ne 'print $1 if /^COUNT (.*)/'))"
fi

echo "== (3) frontmatter parses as YAML =="

# Frontmatter that does not parse means the agent or command does not load at all — a total
# failure that leaves the file looking fine to a reader and to any grep-based check. The
# defect that has actually occurred in this repository: an unquoted YAML plain scalar may
# not contain ": ", so a description ending in "Keywords: ..." silently broke four skills.
#
# Prefer a real parser and say which one ran. A check that quietly degrades to "no parser
# available, nothing to report" is a false green.
yaml_mode=""
if command -v ruby >/dev/null 2>&1 && ruby -ryaml -e 'exit 0' >/dev/null 2>&1; then
  yaml_mode="ruby"
  yaml_out="$(ruby -ryaml -e '
files = ARGV.flat_map { |d| Dir.glob(File.join(d, "*.md")) }.sort
abort("no prompt files found") if files.empty?
files.each do |f|
  label = "#{File.basename(File.dirname(f))}/#{File.basename(f)}"
  text = File.read(f, encoding: "UTF-8")
  unless text.start_with?("---")
    puts "#{label}: no frontmatter"; next
  end
  e = text.index("\n---", 3)
  if e.nil?
    puts "#{label}: unterminated frontmatter"; next
  end
  begin
    d = YAML.safe_load(text[3...e])
    puts "#{label}: frontmatter is not a mapping" unless d.is_a?(Hash)
  rescue => ex
    puts "#{label}: #{ex.message.gsub("\n", " ")[0, 120]}"
  end
end' "$agents_dir" "$commands_dir" 2>&1)"
  yaml_status=$?
else
  yaml_mode="python"
  yaml_out="$(
    python3 - "$agents_dir" "$commands_dir" <<'PY'
import glob, os, sys
try:
    import yaml
except ImportError:
    raise SystemExit('neither ruby/yaml nor python yaml is available to parse frontmatter')
files = sorted(f for d in sys.argv[1:] for f in glob.glob(os.path.join(d, '*.md')))
if not files:
    raise SystemExit('no prompt files found')
for f in files:
    label = os.path.basename(os.path.dirname(f)) + '/' + os.path.basename(f)
    text = open(f, encoding='utf-8', errors='replace').read()
    if not text.startswith('---'):
        print(f'{label}: no frontmatter'); continue
    end = text.find('\n---', 3)
    if end < 0:
        print(f'{label}: unterminated frontmatter'); continue
    try:
        d = yaml.safe_load(text[3:end])
        if not isinstance(d, dict):
            print(f'{label}: frontmatter is not a mapping')
    except Exception as e:
        print(f'{label}: {str(e)[:120]}')
PY
  )"
  yaml_status=$?
fi
if [ $yaml_status -ne 0 ]; then
  fail "frontmatter parse check did not run ($yaml_mode)" "$yaml_out"
elif [ -z "$yaml_out" ]; then
  pass "every agent and command frontmatter parses (via $yaml_mode)"
else
  fail "frontmatter does not parse — these prompts will not load" "$yaml_out"
fi

echo "== (4) resident context stays small =="

# CLAUDE.md is resident in every session. Measured at 8,234 tokens before the redesign, it
# was the single largest fixed cost in the context window, and most of it was procedure that
# only a fraction of tasks need. The budget is a line count because that is what a reader
# and a diff can both check.
claude_md="$here/CLAUDE.md"
if [ ! -f "$claude_md" ]; then
  fail "CLAUDE.md is missing" "expected at $claude_md"
else
  claude_lines="$(wc -l <"$claude_md" | tr -d ' ')"
  if [ "$claude_lines" -le "$claude_md_max_lines" ]; then
    pass "CLAUDE.md is $claude_lines lines (budget $claude_md_max_lines)"
  else
    fail "CLAUDE.md exceeds the resident budget" \
      "$claude_lines lines, budget $claude_md_max_lines. Move procedure into a skill and
leave the trigger condition behind, rather than trimming wording."
  fi
fi

echo "== (5) load table points at real skills =="

# The load table is the replacement for inherits=: it names, per task type, the skill the
# model must load with the Skill tool. A name in it that has no directory sends the model
# after something it cannot load — the same silent degradation inherits= produced, in a new
# place.
#
# Names are read from the right-hand column of the markdown table, plus any skill="..."
# attribute. Deliberately NOT from backticks anywhere in the section: the prose around the
# table backticks words like `inherits` and `refs`, which are not skills and must not be
# reported as missing ones. A single-word skill name in the table is only recognised if it
# already exists, so this check catches a typo in a hyphenated name but not in a bare one.
load_out="$(
  python3 - "$claude_md" "$skills_dir" "$commands_dir" "$agents_dir" <<'PY'
import glob, os, re, sys

claude_md, skills, commands, agents = sys.argv[1:5]
text = open(claude_md, encoding='utf-8', errors='replace').read()

m = re.search(r'<load_table[^>]*>(.*?)</load_table>', text, re.S)
if not m:
    print('MISSING')
    raise SystemExit(0)
body = m.group(1)

# Supplied by external flake inputs rather than this directory.
EXTERNAL = {'paredit-cli', 'mcp-builder', 'skill-creator', 'webapp-testing'}
present = {os.path.basename(d) for d in glob.glob(os.path.join(skills, '*')) if os.path.isdir(d)}
if not present:
    raise SystemExit(f'no skill directories under {skills}')
not_skills = {os.path.basename(f)[:-3]
              for d in (commands, agents)
              for f in glob.glob(os.path.join(d, '*.md'))}

candidates = set(re.findall(r'skill="([^"]+)"', body))
for line in body.splitlines():
    line = line.strip()
    if not line.startswith('|') or re.fullmatch(r'[|\-: ]+', line):
        continue                      # not a row, or the header separator
    cells = [c.strip() for c in line.strip('|').split('|')]
    if len(cells) < 2 or cells[0].lower() == 'trigger':
        continue                      # header row
    target = cells[-1]
    # A hyphenated token is a skill name; a bare word is only taken as one if it exists,
    # so ordinary English in the cell cannot manufacture a failure.
    for token in re.findall(r'[a-z0-9]+(?:-[a-z0-9]+)*', target):
        if '-' in token or token in present:
            candidates.add(token)

refs = {t for t in candidates if t not in not_skills}

missing = sorted(r for r in refs
                 if r not in present and r not in EXTERNAL and not r.startswith('aws-'))
resolved = sorted(refs - set(missing))
for r in missing:
    print('BAD ' + r)
print(f'RESOLVED {len(resolved)}')
PY
)"
load_status=$?
if [ $load_status -ne 0 ]; then
  fail "load table check did not run" "$load_out"
elif printf '%s' "$load_out" | grep -q '^MISSING$'; then
  fail "CLAUDE.md has no <load_table> section" \
    "The load table is what replaces inherits=; without it nothing tells the model which
skill to load for which task type."
elif printf '%s' "$load_out" | grep -q '^BAD '; then
  fail "load table names skills that do not exist" \
    "$(printf '%s' "$load_out" | grep '^BAD ' | perl -pe 's/^BAD //')"
else
  resolved="$(printf '%s' "$load_out" | perl -ne 'print $1 if /^RESOLVED (\d+)/')"
  if [ "${resolved:-0}" -ge 4 ]; then
    pass "every skill named in the load table exists ($resolved entries)"
  else
    fail "load table resolves too few skills to be meaningful" \
      "found ${resolved:-0} skill references; name them in backticks or a skill=\"...\"
attribute so this gate can read them."
  fi
fi

echo "== (6) confidentiality =="

# This repository is public. A client or employer token surviving into published prose is a
# leak. The token list is read from outside the tree so it is never published here, text is
# normalized before matching so ordinary prose formatting cannot defeat the gate, and the
# matcher proves it ran by matching a canary and asserting a non-zero file count.
#
# Scope is every git-tracked file in the repository, not just this directory: a leak is a
# leak wherever it is published, and the identifiers that actually had to be removed lived in
# a host directory name, a nix module, and an org document — none of them under ai-prompts.
# Paths are matched as well as contents, because a directory or filename carrying the token
# is published by `git ls-files` just as visibly as a line inside one.
#
# It remains a denylist, which by construction only catches names someone thought of. Human
# review is the real control; this is a backstop.
if [ ! -r "$denylist_file" ]; then
  fail "confidentiality gate could not run" \
    "denylist not readable at: $denylist_file
set AGENT_SKILLS_DENYLIST, or create the file (one token per line).
This check fails closed on purpose: the list is confidential and is never committed here."
else
  leak_out="$(
    python3 - "$denylist_file" "$repo_root" "${here#"$repo_root"/}" <<'PY'
import os, re, subprocess, sys, unicodedata

denylist_path, root, prompts_rel = sys.argv[1], sys.argv[2], sys.argv[3]

# Latin lookalikes for the Cyrillic and Greek characters most often used to evade a text
# filter. Not exhaustive — a denylist never is — but it costs nothing.
HOMOGLYPHS = str.maketrans({
    'а': 'a', 'е': 'e', 'о': 'o', 'р': 'p', 'с': 'c', 'х': 'x', 'у': 'y',
    'і': 'i', 'ѕ': 's', 'ԁ': 'd', 'ᴏ': 'o', 'ɑ': 'a',
    'α': 'a', 'ε': 'e', 'ο': 'o', 'ρ': 'p', 'ι': 'i', 'κ': 'k', 'ν': 'v',
})

def fold(s):
    # Case-fold and strip accents, but KEEP separators. Deleting them outright would splice
    # adjacent words together and manufacture matches.
    s = unicodedata.normalize('NFKD', s)
    s = ''.join(c for c in s if not unicodedata.combining(c))
    return unicodedata.normalize('NFKC', s).casefold().translate(HOMOGLYPHS)

def compile_token(raw):
    core = re.sub(r'[^0-9a-z]+', '', fold(raw))
    if not core:
        return None
    # A short token is matched only as a contiguous run; allowing separators inside a
    # four-character token produces far more noise than it catches.
    glue = '' if len(core) < 6 else '[^0-9a-z]{0,2}'
    body = glue.join(re.escape(c) for c in core)
    return re.compile(r'(?<![0-9a-z])' + body + r'(?![0-9a-z])')

patterns, sources = [], []
for line in open(denylist_path, encoding='utf-8'):
    line = line.strip()
    if not line or line.startswith('#'):
        continue
    p = compile_token(line)
    if p is not None:
        patterns.append(p)
        sources.append(line)
if not patterns:
    raise SystemExit('denylist contains no usable patterns')

# Prove the matcher works on this run. If folding or regex construction breaks, fail loudly
# rather than silently blessing a leak.
for p, raw in zip(patterns, sources):
    core = re.sub(r'[^0-9a-z]+', '', fold(raw))
    if not p.search(core):
        raise SystemExit('canary failed: a denylist token no longer matches itself')
    if len(core) >= 6 and not p.search('-'.join(core)):
        raise SystemExit('canary failed: separator tolerance is broken')
if any(p.search('should add local paths') for p in patterns):
    raise SystemExit('canary failed: matcher fires on ordinary prose')

def matches(text):
    folded = fold(text)
    return bool(folded) and any(p.search(folded) for p in patterns)

# A path is published as plainly as a line of prose, so the same matcher runs over it. Prove
# that separately: content matching could work perfectly while a bug in the path branch left
# a renamed-but-not-renamed-enough directory unreported.
probe_core = re.sub(r'[^0-9a-z]+', '', fold(sources[0]))
if not matches(os.path.join('docs', probe_core + '-note.org')):
    raise SystemExit('canary failed: a denylist token inside a path is not detected')

def safe(rel):
    # Never echo a matching path verbatim: printing it reproduces the very token in a
    # terminal, a log, or a CI transcript. Redact only the components that matched, so the
    # report still says where to look.
    return '/'.join(
        re.sub(r'[0-9A-Za-z]', '*', c) if matches(c) else c
        for c in rel.split('/')
    )

# Tracked files are the published surface. An untracked scratch file in the working tree is
# not published and lies outside the scope of this gate; a tracked one is in scope, wherever
# it lives. NOTE: keep apostrophes out of this heredoc. It sits inside a $(...) substitution,
# and bash 3.2 -- the only bash on macOS, which the shebang resolves to -- scans for the
# closing paren without honouring the quoted heredoc, so one stray apostrophe opens a string
# that swallows the rest of the file. That defect silently disabled checks (6) through (10).
try:
    listing = subprocess.run(['git', '-C', root, 'ls-files', '-z'],
                             stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                             check=True).stdout
    mode = 'git ls-files'
    rels = [p.decode('utf-8', 'replace') for p in listing.split(b'\0') if p]
except (OSError, subprocess.CalledProcessError):
    # Running from an export rather than a checkout. Walk instead, rather than failing: the
    # fail-closed condition this gate cares about is a missing denylist, not a missing .git.
    mode = 'filesystem walk'
    skip = {'.git', 'result', '.direnv', '.devenv', '.worktrees', 'node_modules'}
    rels = []
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in skip]
        rels += [os.path.relpath(os.path.join(dirpath, n), root) for n in filenames]

scanned, prompts_seen, hits = 0, 0, []
for rel in rels:
    if rel == prompts_rel or rel.startswith(prompts_rel + '/'):
        prompts_seen += 1
    if matches(rel):
        hits.append(f'{safe(rel)}: the path itself matches a denylist token')
    try:
        text = open(os.path.join(root, rel), encoding='utf-8', errors='replace').read()
    except (OSError, ValueError):
        continue                      # tracked but deleted, or unreadable; path still checked
    scanned += 1
    for i, line in enumerate(text.splitlines(), 1):
        if matches(line):
            hits.append(f'{safe(rel)}:{i}: matches a denylist token')

if scanned == 0:
    raise SystemExit(f'scanned 0 files under {root} via {mode}: the gate read nothing')
# The prompt tree is what this suite exists for. If the repo-wide listing somehow excludes
# it, the gate is broader on paper and narrower in fact.
if prompts_seen == 0:
    raise SystemExit(f'scanned {scanned} files via {mode} but none under {prompts_rel}: '
                     'the prompt tree fell out of scope')
for h in hits:
    print(h)
print(f'SCANNED {scanned} tracked files via {mode}')
PY
  )"
  leak_status=$?
  leak_hits="$(printf '%s\n' "$leak_out" | grep -v '^SCANNED ')"
  leak_scope="$(printf '%s\n' "$leak_out" | perl -ne 'print $1 if /^SCANNED (.*)/')"
  if [ $leak_status -ne 0 ]; then
    fail "confidentiality gate did not run" "$leak_out"
  elif [ -z "$leak_scope" ]; then
    # The scope line is the gate's proof of work. Without it the scan did not reach its end,
    # and an empty hit list means nothing.
    fail "confidentiality gate reported no scope" "$leak_out"
  elif [ -z "$leak_hits" ]; then
    pass "confidentiality gate returns zero hits ($leak_scope)"
  else
    # Deliberately does not echo the matched text: printing it would reproduce the very
    # token in a terminal, a log, or a CI transcript. Matching path components arrive
    # already redacted for the same reason.
    fail "client/company identifiers found in published content" "$leak_hits"
  fi
fi

echo "== (7) nix enumerations match the tree =="

# claude-code/default.nix enumerates agents and commands by name in a hardcoded list. A
# file added to the tree but absent from the list is simply never installed, and nothing
# fails — the prompt just does not exist for that tool. A name listed but deleted from the
# tree is the opposite: readFile aborts evaluation. Both are checked here so the first one
# cannot pass silently. opencode/agent-translation.nix instead discovers agents/commands via
# builtins.readDir, so it cannot drift from the tree by construction and is not checked here.
nix_out="$(
  python3 - "$ai_tools_dir" "$agents_dir" "$commands_dir" <<'PY'
import glob, os, re, sys

ai_tools, agents_dir, commands_dir = sys.argv[1], sys.argv[2], sys.argv[3]

on_disk = {
    'agents': {os.path.basename(f)[:-3] for f in glob.glob(os.path.join(agents_dir, '*.md'))},
    'commands': {os.path.basename(f)[:-3] for f in glob.glob(os.path.join(commands_dir, '*.md'))},
}
if not on_disk['agents'] or not on_disk['commands']:
    raise SystemExit('found no agent or command files on disk')

nix_files = [os.path.join(ai_tools, 'claude-code', 'default.nix')]

found_any = False
for nf in nix_files:
    if not os.path.exists(nf):
        print(f'{os.path.relpath(nf, ai_tools)}: file not found')
        continue
    text = open(nf, encoding='utf-8', errors='replace').read()
    for kind in ('agents', 'commands'):
        m = re.search(r'readFiles\s+"[^"]*/' + kind + r'"\s*\[(.*?)\]', text, re.S)
        if not m:
            print(f'{os.path.relpath(nf, ai_tools)}: no readFiles list for {kind}')
            continue
        found_any = True
        listed = set(re.findall(r'"([^"]+)"', m.group(1)))
        label = os.path.relpath(nf, ai_tools)
        for name in sorted(on_disk[kind] - listed):
            print(f'{label}: {kind}/{name}.md exists but is not listed (never installed)')
        for name in sorted(listed - on_disk[kind]):
            print(f'{label}: {kind} list names {name}, which has no file (nix eval will abort)')
if not found_any:
    raise SystemExit('parsed no readFiles lists at all: the check would be vacuous')
PY
)"
nix_status=$?
if [ $nix_status -ne 0 ]; then
  fail "nix enumeration check did not run" "$nix_out"
elif [ -z "$nix_out" ]; then
  pass "claude-code enumeration matches agents/ and commands/ (opencode is readDir-based and structurally exempt)"
else
  fail "nix enumerations disagree with the tree" "$nix_out"
fi

echo "== (8) no tool-call residue =="

# Generated files have previously shipped with tool-call residue pasted at the end. The
# common shape is a write truncated mid-call, which leaves only OPENING tags, so comparing
# "more closes than opens" misses it — compare for equality in both directions instead.
residue_out="$(
  python3 - "$here" <<'PY'
import glob, os, re, sys

files = sorted(glob.glob(os.path.join(sys.argv[1], '**', '*.md'), recursive=True))
if not files:
    raise SystemExit('no files to scan')

NS = r'(?:[A-Za-z][\w.-]*:)?'
BALANCED = ('function_calls', 'invoke', 'parameter', 'content',
            'thinking', 'result', 'system-reminder')
# These have zero legitimate occurrences anywhere in the corpus, so even a balanced pair is
# residue.
NEVER = (r'<' + NS + r'function_calls(?=[\s/>])', r'<' + NS + r'invoke\s+name=')

for f in files:
    text = open(f, encoding='utf-8', errors='replace').read()
    label = os.path.relpath(f, sys.argv[1])
    for tag in BALANCED:
        o = len(re.findall(r'<' + NS + tag + r'(?=[\s/>])', text))
        c = len(re.findall(r'</' + NS + tag + r'\s*>', text))
        if o != c:
            print(f'{label}: <{tag}> x{o} vs </{tag}> x{c}')
    for pat in NEVER:
        if re.search(pat, text):
            print(f'{label}: contains a tool-call tag that has no legitimate use here')
PY
)"
residue_status=$?
if [ $residue_status -ne 0 ]; then
  fail "tool-residue check did not run" "$residue_out"
elif [ -z "$residue_out" ]; then
  pass "no unbalanced tool-residue tags"
else
  fail "unbalanced tags (likely tool residue)" "$residue_out"
fi

echo "== (9) hooks are registered, not merely installed =="

# `programs.claude-code.hooks.<name>` only writes the script into the hooks directory and
# marks it executable. It puts NOTHING in settings.json, so a hook installed that way is
# never invoked. enforce-perl sat inert this way while the transcript corpus counted 1,943
# sed calls it should have blocked. Firing requires a separate settings.hooks.PreToolUse
# entry naming the script.
#
# So this check reads only inside the PreToolUse block. Grepping the file for a hook name
# would match the install line and report ok for precisely the broken state — installed,
# registered nowhere, silent.
#
# rtk-rewrite is deliberately NOT required: it is installed and left unregistered on purpose,
# so it is exempted below rather than demanded. The reason is in the EXEMPT comment.
#
# What this check cannot see: a registered rewriter whose stdout does not match the
# hookSpecificOutput contract is discarded silently, with no error and no exit-code signal.
# The registration count below therefore proves wiring, never behaviour.
reg_out="$(
  python3 - "$ai_tools_dir/claude-code/default.nix" "$here/hooks" "$ai_tools_dir/shared/default.nix" <<'PY'
import glob, os, re, sys

nix_path, hooks_dir, shared_path = sys.argv[1], sys.argv[2], sys.argv[3]
if not os.path.exists(nix_path):
    raise SystemExit('claude-code/default.nix not found at ' + nix_path)
text = open(nix_path, encoding='utf-8', errors='replace').read()

# REQUIRED used to restate the guardrailHookNames list from shared/default.nix as its own
# hardcoded literal. The two could drift silently: a guardrail hook added to guardrailHookNames
# (and wired into claude-code/default.nix, which asserts against that same list) would leave this
# check verifying only the original names forever, still green. Parse the roster out of the shared
# file instead, and fail loudly on a broken parse rather than falling back to an empty REQUIRED
# set that would pass this whole check vacuously.
if not os.path.exists(shared_path):
    raise SystemExit('shared/default.nix not found at ' + shared_path +
                      ': cannot derive the guardrail hook roster')
shared_text = open(shared_path, encoding='utf-8', errors='replace').read()
m = re.search(r'guardrailHookNames\s*=\s*\[(.*?)\]', shared_text, re.S)
if not m:
    raise SystemExit('could not parse guardrailHookNames = [ ... ] out of ' + shared_path)
REQUIRED = set(re.findall(r'"([^"]+)"', m.group(1)))
if not REQUIRED:
    raise SystemExit('parsed an empty guardrailHookNames list from ' + shared_path +
                      ': that is a broken parse, not an empty roster')
# Hooks allowed to be installed without being registered. An entry here silences the
# inert-hook complaint for one name, so add one only with its reason written beside it.
#
# rtk-rewrite: unregistered on purpose. The script itself is fixed and its behaviour is
# tested by section (10), but rtk is not on PATH for the harness, so every command it
# rewrote would reach the tool as `rtk ...` and die with exit 127 — strictly worse than not
# compressing at all. It stays installed and exempt until that PATH problem is solved; the
# entry comes out of this set and goes back into REQUIRED in the same change that fixes it.
EXEMPT = {'rtk-rewrite'}

def basename(cmd):
    name = cmd.rsplit('/', 1)[-1]
    return name[:-3] if name.endswith('.sh') else name

# Every hook written into the hooks directory by the module.
installed = set(re.findall(r'programs\.claude-code\.hooks\.([A-Za-z0-9_-]+)\s*=', text))
if not installed:
    raise SystemExit('parsed no programs.claude-code.hooks.* entries: the check is vacuous')

# The settings.hooks.PreToolUse list, delimited by bracket depth rather than a regex, so a
# nested list cannot truncate the region and hide an entry.
m = re.search(r'(?<!programs\.claude-code\.)hooks\.PreToolUse\s*=\s*\[', text)
if not m:
    print('MISSING')
    raise SystemExit(0)
start = m.end() - 1
depth, end = 0, None
for i in range(start, len(text)):
    if text[i] == '[':
        depth += 1
    elif text[i] == ']':
        depth -= 1
        if depth == 0:
            end = i
            break
if end is None:
    raise SystemExit('hooks.PreToolUse list is unterminated in ' + os.path.basename(nix_path))
region = text[start:end + 1]

registered = {basename(c) for c in re.findall(r'command\s*=\s*"([^"]+)"', region)}
matchers = set(re.findall(r'matcher\s*=\s*"([^"]*)"', region))

if not registered:
    print('BAD PreToolUse block registers no commands at all')
if 'Bash' not in matchers:
    print('BAD PreToolUse has no matcher = "Bash" (found: '
          + (', '.join(sorted(matchers)) if matchers else 'none') + ')')

for name in sorted(REQUIRED - registered):
    print('BAD ' + name + ' is not registered in settings.hooks.PreToolUse, so it never fires')
for name in sorted(installed - registered - EXEMPT):
    print('BAD ' + name + ' is installed but not registered: it will sit inert')
for name in sorted(registered - installed):
    print('BAD ' + name + ' is registered but no programs.claude-code.hooks entry installs it')

# A registration and an install can agree with each other and still both point at a script
# that no longer exists under hooks/.
on_disk = {os.path.basename(f)[:-3] for f in glob.glob(os.path.join(hooks_dir, '*.sh'))}
for name in sorted(installed - on_disk):
    print('BAD ' + name + ' is installed from hooks/' + name + '.sh, which does not exist')

print('REGISTERED ' + str(len(registered)))
PY
)"
reg_status=$?
if [ $reg_status -ne 0 ]; then
  fail "hook registration check did not run" "$reg_out"
elif printf '%s' "$reg_out" | grep -q '^MISSING$'; then
  fail "claude-code/default.nix has no settings.hooks.PreToolUse block" \
    "Without it every hook is installed and never invoked. Installing a hook via
programs.claude-code.hooks.<name> writes the file only; registration is separate."
elif printf '%s' "$reg_out" | grep -q '^BAD '; then
  fail "hooks are not wired up to fire" \
    "$(printf '%s' "$reg_out" | grep '^BAD ' | perl -pe 's/^BAD //')"
else
  pass "every installed hook is registered under a Bash matcher, wiring only ($(printf '%s' "$reg_out" | perl -ne 'print $1 if /^REGISTERED (\d+)/') commands)"
fi

echo "== hooks parse =="

# A hook that does not parse is not a degraded hook, it is no hook: the harness reports an
# error and the command runs anyway, so the rule it was supposed to enforce is simply gone.
#
# Parsing is checked under /bin/bash specifically. On macOS that is bash 3.2, not the bash
# on PATH, and 3.2 aborts on a heredoc containing a backtick inside a command substitution —
# a construct bash 5 accepts. The mode bit is deliberately not checked: hooks reach the
# harness through builtins.readFile, so the permissions on the file in this tree never
# matter.
hook_problems=""
for h in "$here"/hooks/*.sh; do
  [ -e "$h" ] || continue
  name="hooks/$(basename "$h")"
  shebang="$(head -1 "$h")"
  case "$shebang" in
  '#!'*) : ;;
  *) hook_problems="$hook_problems
$name: no shebang" ;;
  esac
  out="$(/bin/bash -n "$h" 2>&1)" || hook_problems="$hook_problems
$name: does not parse under /bin/bash: $out"
done
hook_count="$(find "$here/hooks" -name '*.sh' -type f 2>/dev/null | wc -l | tr -d ' ')"
if [ "${hook_count:-0}" -lt 2 ]; then
  fail "hooks directory looks empty" "found ${hook_count:-0} hook scripts under $here/hooks"
elif [ -z "$hook_problems" ]; then
  pass "all $hook_count hooks parse under /bin/bash"
else
  fail "hook scripts have problems" "$hook_problems"
fi

echo "== (10) hook behavior =="

# Everything above is static: check (9) parses default.nix as text, and the parse check runs
# `bash -n`. Neither feeds a hook anything, and that gap is not cosmetic. A PreToolUse hook whose
# stdout does not match the hookSpecificOutput contract is discarded by the harness silently — no
# error, no change in exit status, nothing anywhere to grep. rtk-rewrite spent its whole life
# emitting the wrong schema while also corrupting the command it produced into
# `rtk ls -la /tmpls -la /tmp`, and every static check in this file reported ok the entire time.
# So the count in check (9) proves wiring; only this section proves behaviour.
#
# Hermetic by construction. The real rtk is replaced by a stub, and the stub reproduces the one
# property of the binary the hook must not get wrong: rtk 0.45.0 exits 3 while printing a valid
# rewrite and exits 1 with empty output when it has no mapping, contradicting its own --help. A
# stub that exited 0 on success would let both historical defects — `$(rtk rewrite "$c") || exit 0`
# (discards every rewrite) and `$(rtk rewrite "$c" || echo "$c")` (concatenates both strings) —
# look correct here, so the odd exit statuses are the load-bearing part of the fixture.
#
# The hooks under test are read from this tree, never from ~/.claude/hooks: the installed copy is
# a symlink into /nix/store from the last `switch` and cannot reflect an edit made here. Point
# VERIFY_HOOKS_DIR at hooks extracted from another ref (`git show <ref>:...`) to watch these cases
# fail against the unfixed versions; a regression test nobody has seen fail is a claim about a
# fix, not a guard on it.
bh_hooks_dir="${VERIFY_HOOKS_DIR:-$here/hooks}"
bh_expected_cases=10
bh_ran=0
bh_nonempty=0
bh_problems=""
bh_perm_hits=""
bh_tmp=""

bh_note() {
  bh_problems="$bh_problems
$1"
}

# The fixture lives under TMPDIR, not in the repository: a scratch directory inside the tree
# is one `.gitignore` edit away from being committed, and an interrupted run leaves it behind
# where the next `git status` has to explain it. The trap covers the interrupt path; the
# explicit call below covers the normal path and is the one that can still fail the gate,
# because it runs before the pass/fail decision. A cleanup that silently fails is how a
# leaked fixture becomes invisible.
bh_cleanup() {
  [ -n "${bh_tmp:-}" ] || return 0
  rm -rf "${bh_tmp:?}" 2>/dev/null || return 1
  bh_tmp=""
}
trap bh_cleanup EXIT INT TERM

# Runs one hook copy against one payload. stdout, stderr and exit status are three separate
# surfaces here and are kept apart: rtk-rewrite speaks on stdout and always exits 0, while
# block-destructive-git speaks on stderr and answers with the exit status alone.
bh_run() {
  bh_out="$(printf '%s' "$2" | /bin/bash "$1" 2>"$bh_tmp/stderr")"
  bh_status=$?
  bh_err="$(cat "$bh_tmp/stderr" 2>/dev/null)"
  if [ -n "$bh_out" ]; then
    bh_nonempty=$((bh_nonempty + 1))
    # A permissionDecision would auto-approve whatever command it names, and this hook cannot
    # tell an already-approved command from one still awaiting confirmation. Checked on every
    # non-empty payload rather than on one chosen case.
    case "$bh_out" in
    *permissionDecision*) bh_perm_hits="$bh_perm_hits $3" ;;
    esac
  fi
}

# Exact string equality, never a prefix or a substring test: `rtk ls -la /tmpls -la /tmp` starts
# with "rtk " and contains the input, so any weaker comparison passes on the corrupted output
# this case exists to catch.
bh_case_rewrite() {
  bh_ran=$((bh_ran + 1))
  bh_run "$bh_tmp/rtk-rewrite.sh" "$2" "$1"
  bh_got="$(printf '%s' "$bh_out" | jq -r '.hookSpecificOutput.updatedInput.command // ""' 2>/dev/null)"
  bh_evt="$(printf '%s' "$bh_out" | jq -r '.hookSpecificOutput.hookEventName // ""' 2>/dev/null)"
  if [ "$bh_got" != "$3" ]; then
    bh_note "$1: updatedInput.command is [$bh_got], expected exactly [$3]"
  elif [ "$bh_evt" != "PreToolUse" ]; then
    bh_note "$1: hookSpecificOutput.hookEventName is [$bh_evt], expected PreToolUse"
  elif [ "$bh_status" != "0" ]; then
    bh_note "$1: stdout is correct but the hook exited $bh_status, expected 0"
  fi
}

# stdout and exit status are two independent surfaces and both are asserted. A rewriter that
# stays silent while exiting non-zero is not passing: PreToolUse treats any status other than
# 0 or 2 as a non-blocking error and shows it to the user on every command. Checking stdout
# alone accepts a hook that handles one input and errors on all the rest.
bh_case_silent() {
  bh_ran=$((bh_ran + 1))
  bh_run "$bh_tmp/rtk-rewrite.sh" "$2" "$1"
  if [ -n "$bh_out" ]; then
    bh_note "$1: expected no stdout, got: $(printf '%s' "$bh_out" | head -3 | tr '\n' ' ')"
  elif [ "$bh_status" != "0" ]; then
    bh_note "$1: stdout was empty as expected, but the hook exited $bh_status, expected 0
(stderr: $(printf '%s' "$bh_err" | head -1))"
  fi
}

# $4 is a substring the block message must contain. Exit 2 alone is ambiguous — /bin/bash also
# exits 2 on a syntax error — so a blocking case has to show its reason as well.
bh_case_git() {
  bh_ran=$((bh_ran + 1))
  bh_run "$bh_tmp/block-destructive-git.sh" "$2" "$1"
  if [ "$bh_status" != "$3" ]; then
    bh_note "$1: exit status $bh_status, expected $3 (stderr: $(printf '%s' "$bh_err" | head -1))"
  elif [ -n "${4:-}" ]; then
    case "$bh_err" in
    *"$4"*) : ;;
    *) bh_note "$1: exited $3 but the message does not mention $4" ;;
    esac
  fi
}

bh_rewrite_src="$bh_hooks_dir/rtk-rewrite.sh"
bh_git_src="$bh_hooks_dir/block-destructive-git.sh"
bh_rewrite_bytes="$(wc -c <"$bh_rewrite_src" 2>/dev/null | tr -d ' ')"
bh_git_bytes="$(wc -c <"$bh_git_src" 2>/dev/null | tr -d ' ')"

if ! command -v jq >/dev/null 2>&1; then
  # Both hooks stand aside when they cannot parse their input, so without jq every case would
  # report an empty stdout and a zero exit — nine of the ten would pass having tested nothing.
  fail "hook behavior gate could not run" \
    "jq is not on PATH. Both hooks exit 0 without it, which would turn this gate green
while exercising nothing. It fails closed instead."
elif [ "${bh_rewrite_bytes:-0}" -lt 200 ] || [ "${bh_git_bytes:-0}" -lt 200 ]; then
  # A missing or empty hook is the vacuous-pass trap for this section: /bin/bash on a nonexistent
  # file writes nothing to stdout, so every silent case would pass against nothing at all.
  fail "hook behavior gate has nothing to run" \
    "rtk-rewrite.sh is ${bh_rewrite_bytes:-0} bytes and block-destructive-git.sh is
${bh_git_bytes:-0} bytes under $bh_hooks_dir; expected both to be substantial files."
else
  bh_tmp="$(mktemp -d "${TMPDIR:-/tmp}/verify-hook-behavior.XXXXXX")"

  # Stand-in for rtk 0.45.0. Prints WITHOUT a trailing newline, as the real binary does — that is
  # what turned the old `|| echo "$c"` fallback into a concatenation rather than two lines.
  cat >"$bh_tmp/rtk" <<'STUB'
#!/bin/bash
# Every invocation is recorded before anything else, so the count below is a count of calls
# that reached THIS binary. Without it, "the substitution produced no @RTK_BIN@" is the only
# evidence the stub was used, and that says nothing about which rtk actually ran.
printf '%s\n' "$*" >>"${0}.calls" 2>/dev/null
[ "${1:-}" = "rewrite" ] || exit 1
cmd="${2:-}"
set -f
set -- $cmd
set +f
case "${1:-}" in
rtk)
  # Measured against rtk 0.45.0: `rtk rewrite "rtk ls"` exits 3 and prints `rtk ls` — the
  # input, unchanged. Returning empty here instead would let case (4) exit on the
  # `-z $rewritten` branch and never reach the guard it exists to test.
  printf '%s' "$cmd"
  exit 3
  ;;
ls | tree | grep | rg | diff | find | cat | git | gh | docker | kubectl)
  printf '%s' "rtk $cmd"
  exit 3
  ;;
esac
exit 1
STUB
  chmod +x "$bh_tmp/rtk"

  # The same transformation claude-code/default.nix applies:
  #   builtins.replaceStrings [ "@RTK_BIN@" ] [ "${llmAgentsPkgs.rtk}/bin/rtk" ]
  # so the bytes under test differ from the installed hook only in that one path.
  # The placeholder has to be there BEFORE the substitution, not merely absent after it.
  # Absence afterwards is equally consistent with a hook that hardcodes an rtk path, in which
  # case the perl below is a silent no-op, the real rtk on PATH answers every case, and this
  # whole section reports on a binary it never installed.
  if ! grep -q '@RTK_BIN@' "$bh_rewrite_src"; then
    bh_note "rtk-rewrite.sh contains no @RTK_BIN@ placeholder, so substituting the stub is a
no-op and the cases below would run against whatever rtk the hook names itself"
  fi
  BH_STUB="$bh_tmp/rtk" perl -pe 's{\@RTK_BIN\@}{$ENV{BH_STUB}}g' "$bh_rewrite_src" >"$bh_tmp/rtk-rewrite.sh"
  perl -pe 's{\@RTK_BIN\@}{$ENV{BH_STUB}}g' "$bh_git_src" >"$bh_tmp/block-destructive-git.sh"
  if grep -q '@RTK_BIN@' "$bh_tmp/rtk-rewrite.sh"; then
    bh_note "rtk-rewrite.sh still holds @RTK_BIN@ after substitution: the stub was never wired in"
  fi
  for bh_h in rtk-rewrite block-destructive-git; do
    bh_syn="$(/bin/bash -n "$bh_tmp/$bh_h.sh" 2>&1)" ||
      bh_note "$bh_h.sh does not parse, so every status below is a shell error: $bh_syn"
  done

  # (1) The rewrite actually happens, and the command survives intact.
  bh_case_rewrite "ls is rewritten" \
    '{"tool_name":"Bash","tool_input":{"command":"ls -la /tmp"}}' \
    'rtk ls -la /tmp'

  # (2) rtk has no mapping: stdout empty, no half-formed payload.
  bh_case_silent "unsupported command is left alone" \
    '{"tool_name":"Bash","tool_input":{"command":"python3 script.py"}}'

  # (3) A non-Bash tool must never be touched. The payload carries a command field that WOULD
  # be rewritten under a Bash tool_name, so the only thing keeping this silent is the
  # tool_name guard. A payload without that field exits on the `-z $command` branch instead,
  # and stays green with the tool_name guard deleted.
  bh_case_silent "non-Bash tool is left alone" \
    '{"tool_name":"Read","tool_input":{"command":"ls -la /tmp"}}'

  # (4) Already routed through rtk: rewriting again would nest the proxy inside itself.
  bh_case_silent "rtk-prefixed command is not rewritten twice" \
    '{"tool_name":"Bash","tool_input":{"command":"rtk ls"}}'

  # (5) Inside a pipeline the output feeds another program, not the model. rtk's formatting is
  # not the native tool's, so a rewrite here breaks the consumer and compresses nothing.
  #
  # The head of the pipeline is `ls`, which the allowlist admits. A head outside the allowlist
  # — `cat`, say — would be caught by Exclusion 3 further down whatever the pipeline guard
  # did, so the case would pass with that guard removed and prove nothing about it.
  bh_case_silent "pipeline is not rewritten" \
    '{"tool_name":"Bash","tool_input":{"command":"ls foo | grep bar"}}'

  # (6) rtk DOES map `git push`, so the stub returns a rewrite for this one and the hook has to
  # refuse it on its own. Rewriting changes the string the permission layer matches and the
  # confirmation prompt displays: a permissions.deny rule written against `git push --force`
  # stops matching once the command reaches the tool as `rtk git push --force`, and a destructive
  # command slips past a rule that is still sitting there looking correct.
  bh_case_silent "supported-but-destructive command is not rewritten" \
    '{"tool_name":"Bash","tool_input":{"command":"git push --force origin main"}}'

  # (7) No emitted payload may carry permissionDecision. Asserted against every non-empty output
  # produced above, and requiring at least one, so it cannot pass by nothing having been emitted.
  bh_ran=$((bh_ran + 1))
  if [ "$bh_nonempty" -eq 0 ]; then
    bh_note "no case emitted any stdout: the rewriter never fired, so the silent cases and the
permissionDecision assertion are all vacuous"
  elif [ -n "$bh_perm_hits" ]; then
    bh_note "hook emitted permissionDecision, which auto-approves the command it names:$bh_perm_hits"
  fi

  # (8) The wrapper-stripping regression guard. rtk-rewrite puts `rtk` in front of commands
  # routinely, so the model reproduces the form from its own transcript; without rtk in %WRAPPER
  # this reads as an unclassified command rather than a git one and goes straight through.
  bh_case_git "rtk git stash is blocked" \
    '{"tool_name":"Bash","tool_input":{"command":"rtk git stash"}}' 2 ORCH-P005

  # (9) The unwrapped form still blocks: adding a wrapper must not have moved the classifier off
  # the case it already handled.
  bh_case_git "git stash is blocked" \
    '{"tool_name":"Bash","tool_input":{"command":"git stash"}}' 2 ORCH-P005

  # (10) Stripping rtk must not make its non-git subcommands look destructive.
  bh_case_git "rtk ls is allowed" \
    '{"tool_name":"Bash","tool_input":{"command":"rtk ls -la"}}' 0

  if [ "$bh_ran" -ne "$bh_expected_cases" ]; then
    bh_note "ran $bh_ran cases, expected $bh_expected_cases: a case was dropped or never reached"
  fi

  # The stub has to have been called. Every case above is consistent with a hook that resolved
  # some other rtk entirely — one on PATH, one at a hardcoded store path — and the outputs
  # would look much the same, because the real binary rewrites the same commands. A recorded
  # call is the only thing that distinguishes "the substitution was consulted" from "the
  # substitution was installed and ignored". Asserted as non-zero rather than as an exact
  # figure, so re-ordering the cases does not turn a wiring assertion into a brittle one.
  bh_calls="$(wc -l <"$bh_tmp/rtk.calls" 2>/dev/null | tr -d ' ')"
  if [ "${bh_calls:-0}" -eq 0 ]; then
    bh_note "the rtk stub was never invoked: the hook reached some other binary, so nothing
above describes the stub's behaviour"
  fi

  bh_cleanup || bh_note "could not remove the fixture directory $bh_tmp: it is still on disk"

  if [ -z "$bh_problems" ]; then
    pass "hooks behave to contract on fed JSON ($bh_ran cases, $bh_nonempty payload(s) emitted, ${bh_calls:-0} stub call(s))"
  else
    fail "hook behavior does not match the contract" "$bh_problems"
  fi
fi

echo "== (11) this gate parses, and cannot be silently truncated again =="

# This file spent an unknown stretch exiting 2 partway through. A single apostrophe in a comment
# inside a python heredoc -- nested in a $(...) substitution -- made bash 3.2 read the quote as a
# string opener and swallow the rest of the file. Checks (6) through (10) never ran. Nothing said
# so: the surviving checks still printed ok, and a caller reading the status through a pipe gets
# the pipeline tail. bash 3.2 is not incidental here, it is the only bash on macOS and the one
# /usr/bin/env resolves to.
# An earlier draft of this check tried to spot the shape instead of the failure: find a heredoc
# nested in a command substitution, then flag an odd apostrophe count inside it. It was wrong. The
# nesting test counted every ")" in the file, including the ones closing a case pattern or a
# function header, so the running depth went negative and the test never fired. Injecting the real
# defect into a hook proved it: the hook stopped parsing and the check still reported ok.
#
# So this asks the question directly instead. bash 3.2 IS what runs these -- it is the only bash on
# macOS and what /usr/bin/env resolves to -- which makes `bash -n` under that binary the ground
# truth rather than a proxy for it. Every shell script in the tree is covered, gates included: the
# gates were the ones nothing checked, and one of them had been dead for an unknown stretch.
parse_problems=""
parse_count=0
for s in "${BASH_SOURCE[0]}" "$ai_tools_dir/agent-skills/verify-skills.sh" \
  "$ai_tools_dir/codex/verify-codex-hooks.sh" "$here"/scripts/*.sh; do
  [ -f "$s" ] || continue
  parse_count=$((parse_count + 1))
  if ! out="$(/bin/bash -n "$s" 2>&1)"; then
    parse_problems="$parse_problems
$(basename "$s"): $(printf '%s' "$out" | head -2 | tr '\n' ' ')"
  fi
done
if [ "$parse_count" -lt 3 ]; then
  fail "script-parse check found almost nothing to parse" \
    "checked $parse_count scripts; expected at least 3 (this gate, verify-skills, verify-codex-hooks)"
elif [ -n "$parse_problems" ]; then
  fail "a gate script does not parse under /bin/bash, so it dies partway and reports nothing" \
    "$parse_problems
A script that stops here still prints the ok lines it already emitted, and a caller reading the
status through a pipe sees the pipeline tail. Check the summary line, not the last ok."
else
  pass "all $parse_count gate scripts parse under /bin/bash $(/bin/bash --version | perl -ne 'print $1 if /version (\S+)/')"
fi

echo "== (12) no numeric self-assessment survives =="

# CLAUDE.md forbids a numeric self-assessment, and commands/define.md blocks it as a critical
# prohibition -- but the rule had drifted into four skills that still prescribed 0-100 feasibility
# scales and 80/60 confidence thresholds. Neither this gate nor verify-skills.sh could see it: both
# resolve a skill NAME to a directory and never read the body. That is the gap this check closes.
#
# There is deliberately no negative filter. One used to strip any matched LINE containing a word
# like "forbidden" or "prohibit" -- but that swallowed the whole line, not just the prohibition
# text, so "Other agents are forbidden from using the old workflow; instead Rate confidence (0-100)
# here." matched the positive pattern and then vanished, violation and all. Confirmed before
# deleting it: the positive pattern alone, run with no filter against ai-prompts/ and
# agent-skills/skills/, returns zero hits -- the corpus's actual prohibition text (CLAUDE.md, this
# file's own comments, core-patterns CORE-P001, define.md DEF-P004) does not contain any of the
# positive alternatives below, so nothing needs excluding.
#
# "out of 100" is bounded to exclude a closing quote immediately after the match. POSIX ERE (this
# grep -E) has no lookahead, so `([^"]|$)` does the same job: agent-skills/skills/workflow-patterns/
# SKILL.md itself explains this very prohibition with `"how good is this out of 100" does not` as a
# REJECTED example, immediately followed by a closing quote -- an unbounded "out of 100" would flag
# the rejection alongside the two real prescriptions it was added to catch
# (agent-skills/skills/quality-tools/SKILL.md:284 and :290).
score_hits="$(
  grep -rnE '\(0-100\)|>0-100<|confidence below [0-9]|Rate confidence|[0-9]{2}\+: Verified|out of 100([^"]|$)|confidence level' \
    "$here" "$skills_dir" --include='*.md' 2>/dev/null
)"
score_scanned="$(find "$here" "$skills_dir" -name '*.md' -type f 2>/dev/null | wc -l | tr -d ' ')"
if [ "${score_scanned:-0}" -lt 40 ]; then
  fail "numeric-self-assessment check had almost nothing to read" \
    "found ${score_scanned:-0} markdown files across the prompt and skill trees; expected 40+"
elif [ -z "$score_hits" ]; then
  pass "no numeric self-assessment scale in the corpus ($score_scanned markdown files)"
else
  n="$(printf '%s\n' "$score_hits" | wc -l | tr -d ' ')"
  fail "a numeric self-assessment scale is prescribed in $n place(s)" \
    "$(printf '%s\n' "$score_hits" | head -10 | perl -pe "s{^\Q$repo_root\E/}{}")
State the observable condition instead -- which capability was found at which file:line, and which
was not found and where it was searched for."
fi

echo "== (13) frontmatter is exactly four lines =="

# Check (2) already pins index 3 to '---', so a fifth frontmatter line no longer produces the
# failure this check originally targeted: the old hardcoded `lib.drop 4` left a stray --- at the
# top of the generated body on a 5-line frontmatter, but shared.parseFrontmatter (see
# shared/default.nix) derives the body from the position of the CLOSING --- instead of a fixed
# drop count, so it does not reproduce that defect. Checked directly: neither codex/default.nix nor
# opencode/agent-translation.nix reads a frontmatter line by fixed index at all — both look a line
# up by prefix via shared.findLineWithPrefix ("description: ", "name: "), which tolerates a
# frontmatter of any length as long as the wanted line is present somewhere in it. Claude Code's
# own consumer (claude-code/default.nix) does not parse frontmatter at all; it reads each file
# verbatim and lets the Claude Code CLI parse it. So no converter's correctness actually depends on
# this shape any more, and check (2)'s index-3 pin already catches what this check was written to
# catch.
#
# What is left: this is now a convention check, not a defect guard. It holds every command and
# agent to the same four-line frontmatter shape so the corpus stays uniform and check (2)'s own
# fixed-index reads — which do still assume index 3 is the close — stay a description of the whole
# corpus rather than of most of it. Kept as its own check, rather than folded into check (2), so a
# future loosening of check (2)'s indices does not silently also loosen this invariant.
fm_out="$(
  python3 - "$commands_dir" "$agents_dir" <<'PY'
import glob, os, sys

seen, bad = 0, []
for d in sys.argv[1:]:
    for f in sorted(glob.glob(os.path.join(d, '*.md'))):
        seen += 1
        label = os.path.basename(os.path.dirname(f)) + '/' + os.path.basename(f)
        lines = open(f, encoding='utf-8', errors='replace').read().split('\n')
        if not lines or lines[0].strip() != '---':
            bad.append(f'{label}: does not open with ---')
            continue
        close = next((i for i, l in enumerate(lines[1:], 1) if l.strip() == '---'), None)
        if close is None:
            bad.append(f'{label}: frontmatter is never closed')
        elif close != 3:
            bad.append(f'{label}: closing --- is on line {close + 1}, expected line 4')
if seen == 0:
    raise SystemExit('found no command or agent files: the check would be vacuous')
print('\n'.join(bad))
print(f'SEEN {seen}')
PY
)"
fm_status=$?
fm_bad="$(printf '%s\n' "$fm_out" | grep -v '^SEEN ')"
if [ $fm_status -ne 0 ]; then
  fail "frontmatter length check did not run" "$fm_out"
elif [ -n "$fm_bad" ]; then
  fail "frontmatter is not exactly four lines" \
    "$fm_bad
The shared parser derives the body from the closing ---, but codex and opencode both assert on
fixed indices. A fifth line corrupts the generated body without failing any other check."
else
  pass "every command and agent frontmatter closes on line 4 ($(printf '%s' "$fm_out" | perl -ne 'print $1 if /^SEEN (\d+)/') files)"
fi

echo "== (14) the shared nix module is the only definition =="

# MCP servers, the dangerous-Bash deny list, the guardrail hook roster, and the frontmatter parser
# were each written out separately per tool, and two of them had already drifted in production:
# codex was missing metabase-mcp entirely, and opencode was missing seven deny patterns including
# rm -rf on the ssh directory. They now live in shared/default.nix. This check is what stops a tool
# from quietly reintroducing its own copy.
shared_out="$(
  python3 - "$ai_tools_dir" <<'PY'
import os, re, sys

root = sys.argv[1]
shared = os.path.join(root, 'shared', 'default.nix')
if not os.path.exists(shared):
    raise SystemExit('shared/default.nix is missing: the single definition it holds is gone')

consumers = {
    'claude-code/default.nix': ['mcpServers', 'bashDenyPatterns'],
    'codex/default.nix': ['mcpServers', 'guardrailHookNames'],
    # opencode consumes the deny list through bashDenyPatternsOpencode, not bashDenyPatterns
    # directly: shared/default.nix defines it as bashDenyPatterns translated into the pattern
    # spelling opencode itself expects (the same shape mcpServerToOpencode uses for mcpServers),
    # so this is still the single source of truth, just named for what this consumer actually reads.
    'opencode/opencode-config.nix': ['mcpServers', 'bashDenyPatternsOpencode'],
    'opencode/agent-translation.nix': ['parseFrontmatter'],
}
problems, checked = [], 0
for rel, attrs in consumers.items():
    path = os.path.join(root, rel)
    if not os.path.exists(path):
        problems.append(f'{rel}: not found')
        continue
    text = open(path, encoding='utf-8', errors='replace').read()
    checked += 1
    if not re.search(r'\bshared\b', text):
        problems.append(f'{rel}: never references the shared module')
    for a in attrs:
        # A bare substring test on the attribute NAME is true by construction: every consumer
        # declares its own option of that same name (claude-code/default.nix has both
        # `programs.claude-code.mcpServers = ...` and, separately, `shared.mcpServers`), so `a not
        # in text` can never fail. Require the dotted form actually read off the shared module,
        # bounded with \b so `shared.bashDenyPatterns` does not also match a longer sibling name
        # like `shared.bashDenyPatternsOpencode`.
        if not re.search(r'\bshared\.' + re.escape(a) + r'\b', text):
            problems.append(f'{rel}: does not consume shared.{a} (no dotted shared.{a} reference found)')

# A local re-declaration is the drift this check exists to catch. serena is the marker: it is a
# stdio server whose command lives in the shared module and nowhere else. Matching only inside a
# `serena = { ... start-mcp-server ... }` block used to require a `[^}]*` class that stops at the
# first `}` -- which is exactly the `}` closing `${nurPkgs.serena}` in a verbatim copy-paste of the
# serena block already declared in the shared module, the one scenario this check exists to catch.
# Search for the marker directly instead: none of the consumer files below is shared/default.nix
# itself (that is the one place `start-mcp-server` legitimately lives), so a plain substring search
# cannot be defeated by an interpolation the brace-spanning class choked on, and cannot
# false-positive against a correct tree either.
for rel in list(consumers) + ['codex/default.nix']:
    path = os.path.join(root, rel)
    if os.path.exists(path):
        text = open(path, encoding='utf-8', errors='replace').read()
        if 'start-mcp-server' in text:
            problems.append(f'{rel}: declares its own serena server again (start-mcp-server marker found outside shared/default.nix)')
if checked < 4:
    raise SystemExit(f'only {checked} of 4 consumers were readable: the check would be weak')
print('\n'.join(problems))
print(f'CHECKED {checked}')
PY
)"
shared_status=$?
shared_bad="$(printf '%s\n' "$shared_out" | grep -v '^CHECKED ')"
if [ $shared_status -ne 0 ]; then
  fail "shared-module check did not run" "$shared_out"
elif [ -n "$shared_bad" ]; then
  fail "a tool no longer derives from the shared module" "$shared_bad"
else
  pass "all $(printf '%s' "$shared_out" | perl -ne 'print $1 if /^CHECKED (\d+)/') consumers derive from shared/default.nix"
fi

echo
printf '%s/%s checks passed\n' "$((checks - failures))" "$checks"
[ "$failures" -eq 0 ] || exit 1
