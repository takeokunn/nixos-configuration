#!/bin/bash
# Claude Code hook that makes the prose skills mandatory instead of advisory. A load_table row in
# CLAUDE.md is prose and fires only when the model remembers it; this gate reads the transcript.
#
#   PreToolUse Write|Edit|MultiEdit on a prose file: technical-writing must be loaded, and yomiyasu
#     too when the new text contains kana.
#   PreToolUse Bash running `gh pr|issue create|edit|comment`: technical-writing and cold-read, since
#     the body is published the moment the command runs. yomiyasu too when the command contains kana.
#   Stop: cold-read must have been loaded after the last prose edit of the main thread.
#
# A subagent is judged by its own transcript, because a skill the main thread loaded is not in a
# fresh subagent's context. A fork inherits that context, so a fork also counts the main transcript.
# Stop blocks once: on the retry Claude Code sets stop_hook_active, and the gate stands aside so a
# session that cannot dispatch a reviewer still ends.

set -euo pipefail

input=$(cat)
command -v jq &>/dev/null || exit 0

field() { printf '%s' "$input" | jq -r "$1 // \"\""; }

event=$(field .hook_event_name)
transcript=$(field .transcript_path)
agent_id=$(field .agent_id)

# Unreadable transcript: nothing to judge against, and blocking would wedge the session.
[[ -n $transcript && -r $transcript ]] || exit 0

transcripts=("$transcript")
if [[ -n $agent_id ]]; then
  session_dir=${transcript%.jsonl}/subagents
  agent_file=$session_dir/agent-$agent_id.jsonl
  if [[ -r $agent_file ]]; then
    transcripts=("$agent_file")
    if jq -e '.isFork == true' "$session_dir/agent-$agent_id.meta.json" &>/dev/null; then
      transcripts+=("$transcript")
    fi
  fi
fi

loaded() {
  grep -qF "\"name\":\"Skill\",\"input\":{\"skill\":\"$1\"" "${transcripts[@]}"
}

# Durable prose only. Memory stores, plans, and scratch space are notes for the agent itself.
is_prose_path() {
  case $1 in
  */.claude/projects/*/memory/* | */.serena/memories/* | */.claude/plans/* | /tmp/* | /private/tmp/* | /var/folders/*) return 1 ;;
  *.md | *.markdown | *.mdx | *.org | *.txt | *.rst | *.adoc) return 0 ;;
  *) return 1 ;;
  esac
}

has_kana() {
  printf '%s' "$1" | perl -CS -Mutf8 -0ne 'exit(/[\p{Hiragana}\p{Katakana}]/ ? 0 : 1)'
}

missing=()
require() { loaded "$1" || missing+=("$1"); }

case $event in
PreToolUse)
  tool=$(field .tool_name)
  case $tool in
  Write | Edit | MultiEdit)
    is_prose_path "$(field .tool_input.file_path)" || exit 0
    text=$(field '[.tool_input.content, .tool_input.new_string, (.tool_input.edits // [] | .[].new_string)] | map(select(. != null)) | join("\n")')
    require technical-writing
    if has_kana "$text"; then require yomiyasu; fi
    ;;
  Bash)
    cmd=$(field .tool_input.command)
    # Anchored to a command position, so a grep or echo quoting the phrase is not a publish.
    printf '%s' "$cmd" | perl -0ne 'exit(/(?:\A|[;&|(\n])\s*gh\s+(?:pr|issue)\s+(?:create|edit|comment)\b/ ? 0 : 1)' || exit 0
    require technical-writing
    require cold-read
    if has_kana "$cmd"; then require yomiyasu; fi
    ;;
  *) exit 0 ;;
  esac
  ((${#missing[@]})) || exit 0
  {
    echo "❌ Prose skills not loaded: ${missing[*]}"
    echo
    echo "Durable prose (docs, README, skill and prompt files, PR and issue bodies) is written with"
    echo "technical-writing, Japanese prose is revised with yomiyasu, and a cold-read pass runs before"
    echo "anything is published. Load the missing skills with the Skill tool, then retry this call:"
    for s in "${missing[@]}"; do echo "  Skill(skill: \"$s\")"; done
  } >&2
  exit 2
  ;;
Stop)
  [[ $(field .stop_hook_active) == true ]] && exit 0
  # Line index of the last prose edit and of the last cold-read load, in transcript order.
  read -r last_edit last_read < <(
    perl -MJSON::PP -ne '
        BEGIN { ($e, $r) = (0, 0) }
        next unless /"name":"(?:Write|Edit|MultiEdit|Skill)"/;
        my $d = eval { decode_json($_) } or next;
        my $c = ($d->{message} // {})->{content};
        next unless ref $c eq "ARRAY";
        for my $b (@$c) {
          next unless ref $b eq "HASH" && ($b->{type} // "") eq "tool_use";
          my $i = $b->{input} // {};
          if ($b->{name} eq "Skill" && ($i->{skill} // "") eq "cold-read") { $r = $. }
          elsif ($b->{name} =~ /^(?:Write|Edit|MultiEdit)$/) { push @p, [$., $i->{file_path} // ""] }
        }
        END {
          for (@p) { $e = $_->[0] if main::prose($_->[1]) }
          print "$e $r\n";
        }
        sub prose {
          my $f = shift;
          return 0 if $f =~ m{/\.claude/projects/[^/]+/memory/|/\.serena/memories/|/\.claude/plans/|^/tmp/|^/private/tmp/|^/var/folders/};
          return $f =~ /\.(?:md|markdown|mdx|org|txt|rst|adoc)\z/;
        }
      ' "$transcript"
  )
  ((last_edit > last_read)) || exit 0
  jq -n '{decision: "block", reason: "Prose files were edited after the last cold-read. Load the cold-read skill with the Skill tool and run its context-free review of the changed prose, apply the surviving cuts, then finish."}'
  ;;
esac
