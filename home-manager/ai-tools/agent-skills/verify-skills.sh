#!/usr/bin/env bash
# Acceptance gate for the custom agent-skills collection and the ai-prompts SSoT.
#
# This repository has no unit-test suite; it is declarative configuration plus prose.
# The equivalent of a test here is: does it still evaluate, and do the invariants that
# cannot be expressed in Nix still hold? Those invariants are checked below.
#
# Usage:  ./verify-skills.sh          # structural checks only (fast)
#         ./verify-skills.sh --eval   # also evaluate a host configuration (slow)
#
# The confidentiality gate needs a denylist of tokens that must never reach this public
# repository. That list is itself confidential — it names clients and employers — so it
# lives outside the tree and is NOT committed. Point AGENT_SKILLS_DENYLIST at it, or place
# it at ~/.config/agent-skills/denylist, one extended-regex alternative per line.
# If the file is absent the gate FAILS rather than passing: a confidentiality check that
# silently skips is worse than no check, because it still reads as green.

set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
skills_dir="$here/skills"
prompts_dir="$here/../ai-prompts"
ai_tools_dir="$here/.."
repo_root="$(cd "$here/../../.." && pwd)"
denylist_file="${AGENT_SKILLS_DENYLIST:-$HOME/.config/agent-skills/denylist}"

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
  [ -n "${2:-}" ] && printf '%s\n' "$2" | sed 's/^/         /'
  return 0
}

echo "== skill structure =="

# Every check below iterates the collection. If the collection were empty — a bad path, a
# rename, a partial checkout — each individual check would find nothing wrong and report
# ok. Assert up front that there is something to check, so the suite cannot pass over
# nothing. This is the same failure mode the test-integrity skill calls a false green.
skill_count="$(find "$skills_dir" -mindepth 1 -maxdepth 1 -type d 2>/dev/null | wc -l | tr -d ' ')"
if [ "${skill_count:-0}" -ge 30 ]; then
  pass "collection is populated ($skill_count skills)"
else
  fail "collection looks empty or truncated" \
    "found ${skill_count:-0} skill directories under $skills_dir; expected at least 30"
fi

# A skill is addressed by its directory name, so the directory must hold a SKILL.md.
# The frontmatter `name` is a display label and is deliberately NOT required to match the
# directory: most skills here use a Title Case label and only the newer ones use the slug.
missing=""
noname=""
for d in "$skills_dir"/*/; do
  name="$(basename "$d")"
  f="$d/SKILL.md"
  if [ ! -f "$f" ]; then
    missing="$missing $name"
    continue
  fi
  grep -q '^name:' "$f" || noname="$noname $name"
done
[ -z "$missing" ] && pass "every skill directory has SKILL.md" ||
  fail "skill directories without SKILL.md" "$missing"
[ -z "$noname" ] && pass "every skill declares a name" ||
  fail "skills missing a name field" "$noname"

# `version` is how a purely-additive edit announces itself; a skill without one cannot be diffed
# against its previous state by a reader.
noversion="$(grep -L '^version:' "$skills_dir"/*/SKILL.md 2>/dev/null)"
[ -z "$noversion" ] && pass "every skill declares a version" ||
  fail "skills missing a version field" "$noversion"

# The description is the only thing the model sees when deciding whether to load a skill.
nodesc="$(grep -L '^description:' "$skills_dir"/*/SKILL.md 2>/dev/null)"
[ -z "$nodesc" ] && pass "every skill declares a description" ||
  fail "skills missing a description field" "$nodesc"

echo "== content integrity =="

# Generated files have previously shipped with tool-call residue pasted at the end of the file.
# `<content>` is a legitimate element in at least one skill, so match only a closing tag that
# has no opening partner in the same file.
residue="$(
  python3 - "$skills_dir" "$prompts_dir" <<'PY'
import glob, os, re, sys
skills, prompts = sys.argv[1], sys.argv[2]
files = glob.glob(os.path.join(skills, '*', 'SKILL.md'))
files += glob.glob(os.path.join(prompts, '**', '*.md'), recursive=True)
files = sorted(set(files))
if not files:
    raise SystemExit('no files to scan')

# Real residue is namespace-prefixed, so the pattern has to allow a prefix. And the common
# shape is a write truncated mid-call, which leaves only OPENING tags — comparing "more
# closes than opens" misses it entirely. Compare for equality in both directions instead.
NS = r'(?:[A-Za-z][\w.-]*:)?'
BALANCED = ('function_calls', 'invoke', 'parameter', 'content',
            'thinking', 'result', 'system-reminder')
# These have zero legitimate occurrences anywhere in the corpus, so a balanced pair is
# still residue. `result` is deliberately absent: at least one skill uses it legitimately.
NEVER = (r'<' + NS + r'function_calls(?=[\s/>])', r'<' + NS + r'invoke\s+name=')

for f in files:
    text = open(f, encoding='utf-8', errors='replace').read()
    for tag in BALANCED:
        o = len(re.findall(rf'<{NS}{tag}(?=[\s/>])', text))
        c = len(re.findall(rf'</{NS}{tag}\s*>', text))
        if o != c:
            print(f'{f}: <{tag}> x{o} vs </{tag}> x{c}')
    for pat in NEVER:
        if re.search(pat, text):
            print(f'{f}: contains a tool-call tag that has no legitimate use here')
PY
)"
# An empty result means "no residue" only if the helper actually ran. A crashed helper also
# prints nothing, so the exit status has to be distinguished from the output.
if [ $? -ne 0 ]; then
  fail "tool-residue check did not run" "$residue"
elif [ -z "$residue" ]; then
  pass "no unbalanced tool-residue tags"
else
  fail "unbalanced closing tags (likely tool residue)" "$residue"
fi

# Markdown escape corruption has been introduced twice before by generation passes.
# Scan the prompt tree too: it is generated by the same means and is just as exposed.
corrupt="$(grep -rlE '\(\\\*|\(\\_|-\\\*-' "$skills_dir" "$prompts_dir" 2>/dev/null)"
[ -z "$corrupt" ] && pass "no markdown escape corruption" ||
  fail "escaped markdown metacharacters found" "$corrupt"

# Frontmatter that does not parse means the skill does not load at all — a total failure
# that leaves the file looking perfectly fine to a reader and to any grep-based check.
# The defect that actually occurred: an unquoted YAML plain scalar may not contain ": ",
# so a description ending in "Keywords: ..." silently broke four skills.
#
# Prefer a real parser and say which one ran. A check that quietly degrades to "no parser
# available, nothing to report" is the false green this collection now has a skill about.
yaml_mode=""
if command -v ruby >/dev/null 2>&1 && ruby -ryaml -e 'exit 0' >/dev/null 2>&1; then
  yaml_mode="ruby"
  yaml_out="$(ruby -ryaml -e '
files = Dir.glob(File.join(ARGV[0], "*", "SKILL.md")).sort
abort("no skill files found") if files.empty?
files.each do |f|
  text = File.read(f, encoding: "UTF-8")
  unless text.start_with?("---")
    puts "#{File.basename(File.dirname(f))}: no frontmatter"; next
  end
  e = text.index("\n---", 3)
  if e.nil?
    puts "#{File.basename(File.dirname(f))}: unterminated frontmatter"; next
  end
  name = File.basename(File.dirname(f))
  fm = text[3...e]
  begin
    d = YAML.safe_load(fm)
    unless d.is_a?(Hash)
      puts "#{name}: frontmatter is not a mapping"; next
    end
    # A space followed by "#" opens a YAML comment, so the rest of the value is dropped
    # with no error at all — the description just silently goes short. Compare what the
    # parser kept against what was written to catch it.
    raw = fm[/^description:[ \t]*(.*(?:\n[ \t]+.*)*)/, 1]
    if raw && d["description"]
      written = raw.split("\n").map(&:strip).join(" ").strip
      written = written[1...-1] if written.start_with?(%q{"}) && written.end_with?(%q{"})
      kept = d["description"].to_s
      if kept.length < written.length - 1
        puts "#{name}: description truncated by YAML (#{written.length} written, #{kept.length} kept) — likely a \" #\" comment"
      end
    end
  rescue => ex
    puts "#{name}: #{ex.message.gsub("\n", " ")[0, 120]}"
  end
end' "$skills_dir" 2>&1)"
  yaml_status=$?
elif python3 -c 'import yaml' >/dev/null 2>&1; then
  yaml_mode="python"
  yaml_out="$(
    python3 - "$skills_dir" <<'PY'
import glob, os, sys, yaml
files = sorted(glob.glob(os.path.join(sys.argv[1], '*', 'SKILL.md')))
if not files:
    raise SystemExit('no skill files found')
for f in files:
    name = os.path.basename(os.path.dirname(f))
    text = open(f, encoding='utf-8').read()
    if not text.startswith('---'):
        print(f'{name}: no frontmatter'); continue
    end = text.find('\n---', 3)
    if end < 0:
        print(f'{name}: unterminated frontmatter'); continue
    fm = text[3:end]
    try:
        d = yaml.safe_load(fm)
        if not isinstance(d, dict):
            print(f'{name}: frontmatter is not a mapping'); continue
        # " #" opens a YAML comment and drops the rest of the value with no error.
        m = re.search(r'^description:[ \t]*(.*(?:\n[ \t]+.*)*)', fm, re.M)
        if m and d.get('description'):
            written = ' '.join(p.strip() for p in m.group(1).split('\n')).strip()
            if written[:1] == '"' and written[-1:] == '"':
                written = written[1:-1]
            kept = str(d['description'])
            if len(kept) < len(written) - 1:
                print(f'{name}: description truncated by YAML '
                      f'({len(written)} written, {len(kept)} kept) — likely a " #" comment')
    except Exception as e:
        print(f'{name}: {str(e).replace(chr(10), " ")[:120]}')
PY
  )"
  yaml_status=$?
else
  # No parser at hand. Detect the one construct that has actually broken this collection
  # rather than reporting success, and say that coverage is reduced.
  yaml_mode="fallback"
  yaml_out="$(
    python3 - "$skills_dir" <<'PY'
import glob, os, re, sys
files = sorted(glob.glob(os.path.join(sys.argv[1], '*', 'SKILL.md')))
if not files:
    raise SystemExit('no skill files found')
for f in files:
    name = os.path.basename(os.path.dirname(f))
    for line in open(f, encoding='utf-8').read().split('\n')[:20]:
        m = re.match(r'^([a-z_]+):[ \t]+(.*)$', line)
        if not m:
            continue
        value = m.group(2)
        if value[:1] in ('"', "'"):
            continue
        if ': ' in value:
            print(f'{name}: unquoted {m.group(1)} contains ": " (breaks YAML)')
        if ' #' in value:
            print(f'{name}: unquoted {m.group(1)} contains " #" (YAML comment truncates it)')
PY
  )"
  yaml_status=$?
fi
if [ $yaml_status -ne 0 ]; then
  fail "frontmatter check did not run ($yaml_mode)" "$yaml_out"
elif [ -z "$yaml_out" ]; then
  pass "every skill's frontmatter parses (via $yaml_mode)"
else
  fail "frontmatter does not parse — these skills will not load" "$yaml_out"
fi

# A description is the only text a model sees when deciding whether to load a skill, and
# the Agent Skills spec caps it at 1024 characters. Over-long descriptions load today but
# are rejected by the collection's own validator, so catch them here.
toolong="$(
  python3 - "$skills_dir" <<'PY'
import glob, os, re, sys
files = sorted(glob.glob(os.path.join(sys.argv[1], '*', 'SKILL.md')))
if not files:
    raise SystemExit('no skill files found')
for f in files:
    text = open(f, encoding='utf-8', errors='replace').read()
    m = re.search(r'^description:[ \t]*(.*(?:\n[ \t]+.*)*)', text, re.M)
    if not m:
        continue
    desc = ' '.join(part.strip() for part in m.group(1).splitlines()).strip()
    if len(desc) > 1024:
        print(f'{os.path.basename(os.path.dirname(f))}: description is {len(desc)} chars (limit 1024)')
PY
)"
status=$?
if [ $status -ne 0 ]; then
  fail "description length check did not run" "$toolong"
elif [ -z "$toolong" ]; then
  pass "every description is within the 1024-character limit"
else
  fail "description exceeds the spec limit" "$toolong"
fi

echo "== confidentiality =="

# This repository is public. Work-organisation material may only appear as anonymized
# generic patterns, so a client or employer token surviving into published content is a leak.
#
# Three things this check does that a plain grep does not:
#   * it reads the token list from outside the tree, so the list is never published here;
#   * it normalizes text before matching (case-folded, accents and homoglyphs folded,
#     separators tolerated inside longer tokens), so for a token `acmecorp` the spellings
#     `ACME-CORP`, `Acme Corp` and a Cyrillic lookalike all match — ordinary prose
#     formatting should not defeat the gate;
#   * it proves it actually ran, by scanning a canary string that it must find, and by
#     asserting a non-zero file count. A gate that scans nothing must not report clean.
#
# It remains a denylist, which by construction only catches names someone thought of.
# It is a backstop. Human review of harvested content is the real control, because
# identification from context alone is not detectable by any token list.
if [ ! -r "$denylist_file" ]; then
  fail "confidentiality gate could not run" \
    "denylist not readable at: $denylist_file
set AGENT_SKILLS_DENYLIST, or create the file (one regex alternative per line).
This check fails closed on purpose: the list is confidential and is never committed here."
else
  leak="$(
    python3 - "$denylist_file" "$ai_tools_dir" <<'PY'
import os, re, sys, unicodedata

denylist_path, root = sys.argv[1], sys.argv[2]

# Latin lookalikes for the Cyrillic and Greek characters most often used to evade a
# text filter. Not exhaustive — a denylist never is — but it costs nothing.
HOMOGLYPHS = str.maketrans({
    'а': 'a', 'е': 'e', 'о': 'o', 'р': 'p', 'с': 'c', 'х': 'x', 'у': 'y',
    'і': 'i', 'ѕ': 's', 'ԁ': 'd', 'ᴏ': 'o', 'ɑ': 'a',
    'α': 'a', 'ε': 'e', 'ο': 'o', 'ρ': 'p', 'ι': 'i', 'κ': 'k', 'ν': 'v',
})

def fold(s):
    # Case-fold and strip accents/compatibility forms, but KEEP separators. Deleting them
    # outright would splice adjacent words together and manufacture matches: an innocent
    # phrase whose word endings and beginnings happen to abut a short token would hit.
    s = unicodedata.normalize('NFKD', s)
    s = ''.join(c for c in s if not unicodedata.combining(c))
    return unicodedata.normalize('NFKC', s).casefold().translate(HOMOGLYPHS)

def compile_token(raw):
    core = re.sub(r'[^0-9a-z]+', '', fold(raw))
    if not core:
        return None
    # A short token is only matched as a contiguous run. Allowing separators inside a
    # four-character token produces far more noise than it catches. Longer tokens tolerate
    # the separators that ordinary prose inserts, e.g. "ACME-CORP" and "Acme Corp".
    glue = '' if len(core) < 6 else '[^0-9a-z]{0,2}'
    body = glue.join(re.escape(c) for c in core)
    # The match must be a whole run, not a fragment of a longer word.
    return re.compile(rf'(?<![0-9a-z]){body}(?![0-9a-z])')

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
    raise SystemExit(f'denylist {denylist_path} contains no usable patterns')

# Prove the matcher works on this run, including the separator tolerance. If folding or
# regex construction breaks, this fails loudly instead of silently blessing a leak.
for p, raw in zip(patterns, sources):
    core = re.sub(r'[^0-9a-z]+', '', fold(raw))
    if not p.search(core):
        raise SystemExit('canary failed: a denylist token no longer matches itself')
    if len(core) >= 6 and not p.search('-'.join(core)):
        raise SystemExit('canary failed: separator tolerance is broken')
if any(p.search('should add local paths') for p in patterns):
    raise SystemExit('canary failed: matcher fires on ordinary prose')

scanned = 0
hits = []
skip_dirs = {'.git', 'node_modules', 'result'}
for dirpath, dirnames, filenames in os.walk(root):
    dirnames[:] = [d for d in dirnames if d not in skip_dirs]
    for name in filenames:
        path = os.path.join(dirpath, name)
        try:
            text = open(path, encoding='utf-8', errors='replace').read()
        except (OSError, ValueError):
            continue
        scanned += 1
        for i, line in enumerate(text.splitlines(), 1):
            folded = fold(line)
            if not folded:
                continue
            for p in patterns:
                if p.search(folded):
                    hits.append(f'{os.path.relpath(path, root)}:{i}: matches a denylist token')
                    break
if scanned == 0:
    raise SystemExit(f'scanned 0 files under {root}: the gate read nothing')
for h in hits:
    print(h)
PY
  )"
  status=$?
  if [ $status -ne 0 ]; then
    fail "confidentiality gate did not run" "$leak"
  elif [ -z "$leak" ]; then
    pass "confidentiality gate returns zero hits"
  else
    # Deliberately does not echo the matched text: printing it would reproduce the
    # very token in a terminal, a log, or a CI transcript.
    fail "client/company identifiers found in published content" "$leak"
  fi
fi

echo "== additive-only edits =="

# Extending an existing skill is supposed to be purely additive: a harvest pass adds
# knowledge, it does not silently rewrite or drop what is already there. Frontmatter
# (`version`, `description`) is exempt because updating it is how an addition announces
# itself. Anything else disappearing is a regression worth a human look.
#
# `--no-ext-diff` is load-bearing, not decoration. This repository configures difftastic
# as an external diff driver, which emits side-by-side structural output with no leading
# `-` markers at all — so a deletion count built on plain `git diff` would read zero for
# every file and this check would be born vacuous.
base="${VERIFY_BASE_REF:-HEAD}"
if ! git -C "$repo_root" rev-parse --verify --quiet "$base" >/dev/null 2>&1; then
  fail "cannot compare against $base" "not a git repository, or the ref does not exist"
else
  changed="$(git -C "$repo_root" --no-pager diff --no-ext-diff --no-color --numstat \
    "$base" -- "$skills_dir" "$prompts_dir" 2>/dev/null | wc -l | tr -d ' ')"
  removed="$(git -C "$repo_root" --no-pager diff --no-ext-diff --no-color -U0 \
    "$base" -- "$skills_dir" "$prompts_dir" 2>/dev/null | python3 -c '
import re, sys

# Classify every removed line. A line whose opening survives inside a longer added line
# was expanded in place, not deleted — the original wording is still there with more
# appended. Trailing punctuation and closing tags shift when a sentence grows, so match on
# a normalized prefix rather than the whole line. Anything with no surviving prefix is a
# genuine removal and needs a human look.
removed, added = [], []
for line in sys.stdin:
    line = line.rstrip("\n")
    if line.startswith("---") or line.startswith("+++"):
        continue
    if line.startswith("-"):
        removed.append(line[1:])
    elif line.startswith("+"):
        added.append(line[1:])

def norm(s):
    return re.sub(r"[^0-9a-z]+", "", s.lower())

exempt = re.compile(r"^\s*(version|description|name):")
added_norm = [norm(a) for a in added]
for r in removed:
    if not r.strip() or exempt.match(r):
        continue
    body = norm(r)
    if not body:
        continue
    # Short lines must survive whole: a handful of characters can reappear by chance.
    # Longer lines are matched on a leading fraction, because a growing sentence pushes
    # its own trailing punctuation and closing tag past the original ending.
    probe = body if len(body) < 15 else body[:min(60, int(len(body) * 0.8))]
    if any(probe in a for a in added_norm):
        continue
    print(r)
' || true)"
  if [ "${changed:-0}" -eq 0 ]; then
    printf '  --   no tracked changes against %s to check\n' "$base"
  elif [ -z "$removed" ]; then
    pass "edits to $changed tracked file(s) are additive (frontmatter aside)"
  else
    fail "existing content was removed, not just added" \
      "$(printf '%s' "$removed" | head -20)"
  fi
fi

echo "== cross-references =="

# ai-prompts composes agent and command definitions out of skill sections via
# inherits="skill#anchor". A dangling anchor silently degrades the prompt instead of erroring,
# so it has to be checked mechanically. This class of breakage has occurred before.
ref_out="$(
  python3 - "$prompts_dir" "$skills_dir" <<'PY'
import re, os, sys, glob
prompts, skills = sys.argv[1], sys.argv[2]

def read(p):
    return open(p, encoding='utf-8', errors='replace').read()

prompt_files = glob.glob(os.path.join(prompts, '**', '*.md'), recursive=True)
skill_files = glob.glob(os.path.join(skills, '*', 'SKILL.md'))
if not prompt_files or not skill_files:
    raise SystemExit('found no prompt or skill files to cross-check')

bad = []

# inherits="skill#anchor" composes agent and command definitions out of skill sections.
refs = {}
for f in prompt_files:
    for m in re.finditer(r'inherits[= ]"?([a-z0-9-]+)#([a-z0-9_]+)', read(f)):
        refs.setdefault((m.group(1), m.group(2)), set()).add(os.path.basename(f))
for (sk, anc), files in sorted(refs.items()):
    p = os.path.join(skills, sk, 'SKILL.md')
    src = ', '.join(sorted(files))
    if not os.path.exists(p):
        bad.append(f'inherits {sk}#{anc} -> no such skill (from {src})')
        continue
    if not re.search(rf'<{re.escape(anc)}[\s>]|id="{re.escape(anc)}"|name="{re.escape(anc)}"', read(p)):
        bad.append(f'inherits {sk}#{anc} -> no such anchor (from {src})')

# defer_to / related_skills name another skill. A typo here silently sends a reader nowhere.
present = {os.path.basename(os.path.dirname(p)) for p in skill_files}
external = {'paredit-cli', 'mcp-builder', 'skill-creator', 'webapp-testing'}
for p in sorted(skill_files):
    owner = os.path.basename(os.path.dirname(p))
    body = read(p)
    for m in re.finditer(r'<(?:defer_to|skill)\s+skill="([a-z0-9-]+)"', body):
        target = m.group(1)
        if target not in present and target not in external:
            bad.append(f'{owner}: defer_to skill="{target}" -> no such skill')
print(f'REFS {len(refs)}')
for b in bad:
    print('BAD ' + b)
PY
)"
status=$?
if [ $status -ne 0 ]; then
  fail "cross-reference check did not run" "$ref_out"
elif printf '%s' "$ref_out" | grep -q '^BAD '; then
  fail "dangling cross-references" "$(printf '%s' "$ref_out" | grep '^BAD ' | sed 's/^BAD //')"
else
  pass "every inherits anchor and defer_to target resolves ($(printf '%s' "$ref_out" | sed -n 's/^REFS //p') inherits refs)"
fi

# Every name advertised in the CLAUDE.md registry must exist on disk, or the model is told
# about a skill it cannot load.
ghost=""
for s in $(grep -o 'skill name="[a-z0-9-]*"' "$prompts_dir/CLAUDE.md" | sed 's/skill name="//;s/"//'); do
  case "$s" in
  aws-* | paredit-cli) continue ;; # supplied by external flake inputs, not this directory
  esac
  [ -d "$skills_dir/$s" ] || ghost="$ghost $s"
done
[ -z "$ghost" ] && pass "registry lists no non-existent skill" ||
  fail "registry advertises skills that do not exist" "$ghost"

echo "== registry coverage (informational) =="

# These six are consumed only through inherits= by agents and commands; they are deliberately
# absent from the user-facing catalogue. Anything else missing from the registry is an omission.
exempt=" define-core exploration-tools mdq parallelization-patterns quality-tools workflow-patterns "
unlisted=""
for d in "$skills_dir"/*/; do
  name="$(basename "$d")"
  case "$exempt" in *" $name "*) continue ;; esac
  grep -q "skill name=\"$name\"" "$prompts_dir/CLAUDE.md" || unlisted="$unlisted $name"
done
[ -z "$unlisted" ] && pass "every user-facing skill is registered in CLAUDE.md" ||
  fail "user-facing skills missing from the registry" "$unlisted"

if [ "${1:-}" = "--eval" ]; then
  echo "== nix evaluation =="
  # The skills are materialised by a Home Manager module composed into the host
  # configurations, so evaluating a host's derivation is what actually proves the
  # collection still evaluates. There is no top-level `homeConfigurations` output.
  attr=""
  for set in darwinConfigurations nixosConfigurations; do
    names="$(cd "$repo_root" && nix eval --no-write-lock-file --json ".#$set" \
      --apply 'builtins.attrNames' 2>/dev/null)" || continue
    host="$(printf '%s' "$names" | python3 -c 'import json,sys; v=json.load(sys.stdin); print(v[0] if v else "")' 2>/dev/null)"
    [ -n "$host" ] || continue
    attr=".#$set.\"$host\".system.drvPath"
    break
  done
  if [ -z "$attr" ]; then
    fail "no host configuration found to evaluate"
  elif out="$(cd "$repo_root" && nix eval --no-write-lock-file --raw "$attr" 2>&1)"; then
    pass "host configuration evaluates ($attr)"
  else
    fail "nix eval failed for $attr" "$out"
  fi
fi

echo
if [ "${1:-}" = "--eval" ]; then
  printf '%s/%s checks passed\n' "$((checks - failures))" "$checks"
else
  printf '%s/%s checks passed (nix evaluation SKIPPED — run with --eval)\n' \
    "$((checks - failures))" "$checks"
fi
[ "$failures" -eq 0 ] || exit 1
