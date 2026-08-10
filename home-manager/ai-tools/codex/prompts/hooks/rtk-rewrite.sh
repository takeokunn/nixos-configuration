#!/bin/bash
# PreToolUse:Bash hook — route a read-only command through rtk so its output is compressed
# before it reaches the model's context.
#
# Only hookSpecificOutput.updatedInput is emitted. A permissionDecision here would auto-approve
# whatever it names, and this hook cannot tell a command the user already approved from one that
# still needs confirmation. A payload outside that shape is discarded silently at exit 0, so a
# regression in the output contract cannot be seen in the exit status — only in observed stdout.
#
# rtk rewrite's exit status is not a success flag and must never be used as one. rtk 0.45.0 exits
# 3 while printing a valid rewrite and 1 with empty output when it has no mapping, contradicting
# its own --help ("Exits 0 ... if supported") and the idiom that help publishes,
# `REWRITTEN=$(rtk rewrite "$CMD") || exit 0` — which silently discards every rewrite. The
# earlier form here, `$(rtk rewrite "$c" || echo "$c")`, fired its fallback on every success and
# concatenated both strings into `rtk ls -la /tmpls -la /tmp`. That corruption was inert only
# because the old output schema was itself being discarded; fixing the schema is what would have
# armed it. Only stdout decides: empty, or equal to the input, means no rewrite.
#
# This hook is deliberately not registered in settings.json, so nothing below currently runs. What
# follows is what has to be fixed before it is registered; each item was measured against this file
# as it stands, and each is inert today for a reason that registration removes.
#
# The emitted rewrite names a bare `rtk` while RTK_BIN holds an absolute store path, so the command
# handed back would not resolve. Registering the hook turns every rewritten command into exit 127,
# and the resulting errors and retries add context instead of saving it. Putting rtk in
# home.packages fixes it; so does emitting the absolute path, but that second form has to be agreed
# with block-destructive-git.sh, whose %WRAPPER unwraps a bare `rtk` and not a store path.
#
# updatedInput replaces tool_input wholesale rather than merging into it, and the object emitted
# here carries only `command`, so timeout, run_in_background and description are dropped. Bash's
# schema requires only `command`, so the payload still validates and the loss is silent: a command
# submitted with run_in_background true would run in the foreground. Building the object from
# `.tool_input` and substituting `.command` into it is the fix.
#
# The find exclusion matches a literal space before each destructive flag, so `find . -name x`
# followed by a tab and `-delete` passes the allowlist. Nothing is destroyed today only because rtk
# 0.45.0 refuses find's action flags itself, which puts the guarantee in rtk rather than here.
#
# The metacharacter exclusion is applied to the input and never to rtk's answer: `$rewritten` is
# tested for its prefix and for an embedded newline, nothing else. The current rtk is a verbatim
# prefixer, so there is nothing to inject — but the paragraph above records rtk already
# contradicting its own --help, so this is a dependence that has been betrayed once.
#
# The `*"/rtk "*` arm is unanchored and matches the substring at any position, so
# `sudo /usr/local/bin/rtk ls` is accepted. Nor is the rewrite checked against the command it
# replaces: a stub answering `rtk read /etc/shadow` to an `ls` input was emitted unchanged.

set -euo pipefail

RTK_BIN="@RTK_BIN@"

input=$(cat)

if ! command -v jq &>/dev/null; then
  exit 0
fi

tool_name=$(printf '%s' "$input" | jq -r '.tool_name // ""' 2>/dev/null || echo "")
event=$(printf '%s' "$input" | jq -r '.hook_event_name // ""' 2>/dev/null || echo "")
command=$(printf '%s' "$input" | jq -r '.tool_input.command // ""' 2>/dev/null || echo "")

if [[ $tool_name != "Bash" ]] || [[ -z $command ]]; then
  exit 0
fi

# The field is absent from a hand-fed payload, so only a stated mismatch disqualifies the event.
if [[ -n $event ]] && [[ $event != "PreToolUse" ]]; then
  exit 0
fi

if [[ ! -x $RTK_BIN ]]; then
  exit 0
fi

normalized="$(printf '%s' "$command" | perl -0pe 's/\A\s+//; s/\s+\z//')"

# Exclusion 1: anything that is not a lone simple command. Inside a pipeline, a command
# substitution or a redirect, the output is consumed by another program rather than read by the
# model, and rtk's formatting differs from the native tool's, so rewriting there breaks the
# consumer and saves no context. Shapes this test cannot decide are left alone.
nl='
'
case $normalized in
*'|'* | *'&'* | *';'* | *'<'* | *'>'* | *'`'* | *'$('* | *'('* | *')'* | *"$nl"*)
  exit 0
  ;;
esac

# Globbing is off so an unquoted pattern such as `ls *.nix` splits into tokens rather than
# expanding against whatever directory the hook happens to run in.
set -f
set -- $normalized
set +f
t1="${1:-}"
t2="${2:-}"
t3="${3:-}"

# Exclusion 2: already routed through rtk. rtk returns such a command unchanged, so the equality
# test below catches it too; this is the explicit guard.
if [[ $t1 == "rtk" ]]; then
  exit 0
fi

# rtk decides whether it has a mapping at all. Its stdout is the only signal read here; `|| true`
# keeps set -e from acting on the non-zero status that accompanies a successful rewrite.
rewritten="$("$RTK_BIN" rewrite "$normalized" 2>/dev/null)" || true

if [[ -z $rewritten ]] || [[ $rewritten == "$normalized" ]]; then
  exit 0
fi

# Exclusion 3: commands rtk does support but that must not be rewritten anyway, because the
# rewrite changes the string the permission layer matches and the confirmation prompt displays.
# rtk maps `git push origin main`, `gh release delete v1`, `aws ec2 terminate-instances ...`,
# `aws s3 rm --recursive`, `psql -c 'DROP TABLE users'`, `curl -X POST ... -d @payload`,
# `pip install`, `prisma migrate deploy` and `uv run <script>`, so "rtk supports it" carries no
# safety of its own, and the set needing exclusion is open — it grows with every rtk release.
#
# It is therefore written as the closed complement: a command survives only if its family is
# named here and, for a family that can also mutate something, its sub-command is a reading one.
# Naming what may pass is the only form of this rule that a new rtk version cannot outgrow.
# Everything else is excluded, costing at most some unrealised compression.
#
# `cat` is excluded on the same grounds: rtk maps it to `rtk read`, a different command that
# filters the file's contents rather than reproducing them.
allowed=0
case $t1 in
ls | tree | grep | rg | diff)
  allowed=1
  ;;
find)
  # find's action flags delete and execute. -exec also covers -execdir, -ok covers -okdir.
  case $normalized in
  *' -delete'* | *' -exec'* | *' -ok'* | *' -fprint'* | *' -fls'*) allowed=0 ;;
  *) allowed=1 ;;
  esac
  ;;
git)
  # branch and tag are omitted: -D and -d make them destructive under the same sub-command.
  case $t2 in
  status | log | diff | show | blame | describe | shortlog | reflog | ls-files | ls-tree | grep) allowed=1 ;;
  esac
  ;;
gh)
  # gh's read/write split sits at the third token, so both are checked. This leaves out `gh api`,
  # which issues whatever HTTP method it is handed.
  case $t2 in
  pr | issue | run | repo | release | workflow)
    case $t3 in
    list | view | status | checks | diff) allowed=1 ;;
    esac
    ;;
  esac
  ;;
docker)
  case $t2 in
  ps | images | logs | top | port | stats | version | info | history | inspect) allowed=1 ;;
  esac
  ;;
kubectl)
  case $t2 in
  get | describe | logs | top | explain | version | api-resources | api-versions | cluster-info) allowed=1 ;;
  esac
  ;;
esac

if [[ $allowed -ne 1 ]]; then
  exit 0
fi

# Nothing downstream re-checks this, so a rewrite that is not a single rtk invocation is dropped
# rather than handed to the tool.
case $rewritten in
"rtk "* | *"/rtk "*) : ;;
*) exit 0 ;;
esac

case $rewritten in
*"$nl"*) exit 0 ;;
esac

jq -n --arg cmd "$rewritten" \
  '{hookSpecificOutput: {hookEventName: "PreToolUse", updatedInput: {command: $cmd}}}'
