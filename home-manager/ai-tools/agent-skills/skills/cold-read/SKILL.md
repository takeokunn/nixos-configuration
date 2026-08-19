---
name: cold-read
description: A context-free reviewer reads durable prose as its real audience; the writer applies surviving cuts. Use at task completion whenever docs, README, comment blocks, commit messages, or PR/issue bodies were written or revised.
version: 1.0.0
---

# Cold read

The writer cannot measure its own prose — the context that produced it makes every sentence feel necessary.
A fresh reader with none of that context measures it instead. Its report is evidence to weigh, not an edit
script to apply verbatim.

## Procedure

1. Collect the task's durable prose. A non-file artifact (a PR body, a commit message) goes into a scratch
   file first, one per artifact.
2. Give the reviewer only what the artifact's real reader will have. For docs and code comments: the working
   tree, never the diff or the conversation that produced it — a comment that only reads clearly next to the
   diff is the defect this test catches. For a PR or commit body: the body plus its diff. Nothing from this
   session.
3. Dispatch one fresh agent via the Agent tool with a subagent_type other than `fork` — a fork inherits this
   conversation's context, which is exactly what this test must exclude. Give it the reviewer prompt below
   verbatim, filling only `{paths}` and the audience line (the artifact's real reader: "a developer new to
   this repo", "the human reviewing this PR"). Anything more is context that reader will not have, and it
   blinds the test.
4. Apply the report: a cut lands unless it drops a reader decision, precondition, or warning — keeping a span
   requires naming that loss, and "adds nuance" is the writer's bias, not a loss. Re-anchor each reported
   confusion to a fact the reader can see; add prose only for a confusion the reviewer actually raised or a
   fact the reader can reach nowhere else. If most of a passage is cut, rewrite it from the survivors rather
   than patching around the cuts. For style-level repairs, apply technical-writing's rules, not the
   reviewer's prose.
5. Load-bearing docs (README, architecture notes, onboarding) get three independent reviewer dispatches: a
   span every one of them cuts is dead weight; a confusion any one of them raises needs an anchor in the text.

## Reviewer prompt template

    You are reading this material for the first time. You know nothing about the
    conversation, task, or commits that produced it. Audience to embody: {audience line}.

    Read: {paths}. You may read other repository files to verify claims, but read no
    git history and, unless a diff is listed above, no diffs.

    Report three lists with file:line spans:
    1. Probe — what is each artifact for, and what would you do differently because
       you read it? Cite only what you read.
    2. Confusions — every place you stopped, reread, guessed, or hit a referent you
       cannot resolve ("the above fix", "now", "this approach").
    3. Cuts — every span whose deletion loses nothing your audience would miss; a span
       earns its place by changing what the reader does or believes. Propose no
       additions; name gaps under Confusions instead.

    Report to an editor, not a person: no praise, no hedging, no summary.

## Related

- [technical-writing](../technical-writing/SKILL.md) — the style rules to apply when acting on a reviewer's
  Confusions or Cuts
