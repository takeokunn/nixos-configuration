---
name: cognitive-rhythm-writing
description: Use when writing or revising Japanese prose that must read as compelling, not just correct - pacing, tension, sentence rhythm, and opening technique. Apply after technical-writing's prose_norms baseline for correctness and rigor.
version: 3.0.0
---

Designs cognitive-mode pacing (observation → hesitation → assertion → re-observation) and unresolved
tension in Japanese explanatory prose, so that dense, technically correct writing also reads as compelling
and keeps the reader turning the page. Apply technical-writing's japanese/prose_norms ruleset first, for
sentence-level correctness, argument rigor, and hygiene; apply this skill's rhythm norms second, for pacing,
tension, and engagement. The two are complementary rather than overlapping — for example, both independently
forbid empty topic-announcement sentences like 「本章ではA、B、Cを扱う」: prose_norms because it is padding
(its llm_tell_avoidance category), this skill because it is an attitude-less agenda table (see Opening design
below).

## Fundamentals

- Pacing is designed as a switch of cognitive mode, not a change in information density. Observation →
  hesitation → assertion → re-observation is one round-trip unit.
- The text always keeps open at least one unresolved tension (an unanswered question, an unverified
  conviction, or an answer promised for later). The moment all tension closes, the reader can stop reading.
- Write in the voice of a participant thinking through the problem, not a voice explaining a finished
  conclusion. The reader's re-enactment of the writer's process of reaching an answer becomes the reader's
  own re-enactment of thinking.
- Generation-side constraint: when writing new sentences, draw material for beat, tension, and looseness
  only from the situation side (events, data, or statements in the depicted world; the narrator's state of
  judgment). Where the situation offers no material at a given position, leave that position flat rather
  than adding anything. Sentences that make the body of the text itself the topic are a violation of this
  norm, not an application of it. Run every newly written or reshaped sentence through the looseness-vs-padding
  test (see below) at the moment it is written.
  - Bad: `〜の列挙はしない`
  - Bad: `問いは〜だけである`
- The devices this norm defines are realized, not declared: do not write the names, procedures, or example
  phrasing of this norm's own devices (「答えの半分」「緊張」「回収」「問いを半分ずつ返す」, etc.) directly
  into the body text. 「問いを半分ずつ返す」is realized by writing the first half of the answer as content,
  not by announcing that the operation is about to happen. See Looseness vs. padding below for the worked
  bad/good examples and the leak test that catches this.
- Scene-free exposition: in commentary or explanatory prose that has no narrative scene, the situation side
  is the nature of the object itself (data, calculations, trade-offs, a naive expectation broken by fact)
  plus the reader's inference or counter-question. Build tension from the object's nature —
  「流暢さと正しさが一致しないのはなぜか」is about the object and is allowed. Do not use narration of the
  text's own progress as a substitute for tension just because there is no scene.
- Prohibition of the short-sentence-shortening bias: do not cut sentences merely to produce a beat. Cutting
  the context an introduction needs to share (scope, viewpoint, axes of comparison, unsettled matters) in
  order to make it shorter is an omission, not pacing. Raise density only on top of context that has already
  been shared.

## Sentence rhythm

- Plant a short sentence as footing, let a longer sentence flow, and stop with a short sentence. "Plant →
  flow → stop" is the paragraph's basic beat.
- Do not push through with assertion alone; alternate assertion and hesitation. Assertion markers:
  「〜だった」「〜である」「〜というわけだ」. Hesitation markers: 「〜に違いない」(a preconception later
  betrayed by fact), 「〜とは思う。ただ…」「〜だろうか」.
- Hesitation is not weakness but a device: a conviction later betrayed by fact becomes a setup that steers
  the reader's prediction before overturning it.
  - Example: 「うまくいっているに違いない。」→(次の段落で)「ところが、あとになって記録を見ると、そうではなかった。」
- At a turning point, use the "concession → turn → short stop" beat. The short directive sentence after the
  turn fixes the reader's gaze.
  - Example: 「〜だろう。これからも〜だろう。しかし、ここで扱うのは〜のほうだ。考えたいのは、そちらである。」

## Paragraph density wave

- After 2-3 dense paragraphs in a row, place one sparse paragraph. Limit the sparse paragraph's function to
  one of: fixing a settled fact in a single line, presenting the next object of judgment, or switching
  viewpoint distance.
- Do not fix the viewpoint distance. Alternate paragraphs that hug the concrete (records, numbers,
  statements, code) with paragraphs that step back one level for meaning-making.
- Bulleted lists function not only to compress information but as a "pause" that halts the body's breathing.
  The one-level-back sentence after a list (「要するに〜」) works precisely because of this pause.

## Opening design

The opening's job is to create one unresolved tension within the first few sentences; the type of opening is
not fixed. Usable types include:

- Restate the reader's own felt sense (「〜と感じることがあるだろう」), then move to a hypothesis
  (「〜が違うのかもしれない」)
- A direct question to the reader — but do not leave it abandoned; answer it immediately in the writer's own
  words
- A confident general proposition that the rest of the body goes on to test
- Affirmatively write out the narrator's own preconception in full, then break it with a scene grounded in
  fact
- Restate, in the participant's own words, a question left open by the previous chapter or section

Preview and summary are not forbidden. One or two sentences carrying an attitude
(「〜を考えるうえで、これほど適切な切り口もないはずだ」「言い換えると〜という話である」) themselves create
tension. What must be forbidden is only an attitude-less agenda table.

- Bad: `本章ではA、B、Cを扱う`

State, in the reader's own words, the resistance the reader will likely feel (old, contrived, impractical,
not relevant to me), dispatch it briefly, then enter the main topic.

Reader voice vs. padding: this category's "direct question to the reader" type and section-entry's
counter-question device surface-resemble the reader-Q&A/reaction-acting pattern that technical-writing's
prose_norms/redundancy category bans (「〜と感じたかもしれない。そのとおりである」). The two rules do not
conflict: prose_norms bans that pattern only when it is decorative padding with zero situation-side
information, while this skill's reader-voice devices are valid only when the reader-voice content itself
passes the looseness-vs-padding test — i.e., it updates the narrator's genuine state of judgment, hesitation,
or emotion, exactly as 「そうしたかった、とは思う」does in the looseness-vs-padding good-example list below.

## Section entry

Do not open a section with a declaration like「本節では〜を扱う」. Instead enter by one of:

- Restate, as the participant's own question, the discomfort left by the previous section
- Write out the counter-question the reader naturally holds (「では、先に〜しておけばよかったのだろうか」);
  do not answer it immediately — first receive it with「そうしたかった、とは思う」, then overturn it
- Open with the writer's own confession (「白状すれば、〜という算段もあった」), used not for self-criticism
  but as footing for the argument that follows (「この算段は半分だけ正しい」)

Reader voice vs. padding: see Opening design's note above — the counter-question device is valid only when
its reader-voice content passes the looseness-vs-padding test, not as a decorative reader-reaction restatement.

Introduce theory, concepts, or quotations only after first creating in the reader a "still-unnamed
discomfort." Theory enters as a naming, not as an answer; presenting the theory first and confirming it with
an example afterward robs the reader of the discovery.

The bridge between sections belongs at the head of the next section, not the tail of the previous one.
Appending a preview to a section's end is progress-narration and is padding; if the next section's head opens
with a counter-question, discomfort, or confession, the reader keeps reading even without a preview.

- Bad: `次は〜を見る。`

## Enumeration landing

- Once properties or classifications are enumerated, do not leave them merely enumerated; land each item,
  one at a time, on the specific concrete scene just discussed.
  - Example: 「一つめは、さっき見た〜の正体である」「二つめにも身に覚えがある」
- Do not keep the landing style uniform across items; vary it — naming the true identity, personal
  recognition, mapping to a specific fact, future renunciation, and so on.

## Question resolution and closing

- A question raised partway through must not be left unaddressed; recover it explicitly. Returning the
  answer in two halves becomes the driving force for the latter half.
  - Example: 「答えの半分がこれである」「残り半分は〜にある」
- The closing lands the accumulated abstraction on a concrete the reader already holds (the opening scene,
  the reader's own experience, an early question) before closing. Do not end while still in abstraction or
  general rule.
- Choose which tensions to close; exactly one may be left open at the very end. Reticence or delegation to
  the reader functions as room for reader participation.
  - Example: 足りない部分は読者が埋めてほしい
- Second-person address, requests to the reader, and the writer's own reticence or disclaimers function as
  looseness only at boundaries (chapter/section openings and closings). Do not mix them into mid-chapter
  argumentation.
  - Example: どうか〜と割り切って読んでほしい

## Looseness vs. padding

The judgment axis is exactly one: does the sentence update the "situation," or the "document"?

- A situation-updating sentence conveys new information about the depicted world's events, data, or a
  person's statement, or about the narrator's state of judgment (preconception, suspension, regret,
  concession, confession). Keep it as looseness.
- A document-updating sentence conveys only how this chapter, section, explanation, or the discussion so far
  "looks," or what will be written next. Delete it in principle.
- Padding typically narrates how the explanation looks to the reader together with the writer's plan for
  what comes next; the topic is the text itself, with zero situation-side information.
  - Bad: `ここまでだと、概念の説明に見えるだろう。なので、すぐに例へ戻す。`
- Do not merely restate the chapter's own character or subject without adding new information about the
  object.
  - Bad: `要するに、この章の主題は〜ではなく〜である。`
- Do not write a disclaimer that fails to specify the misreading it dispels; a vague disclaimer alone is
  padding (compare the reader-pushback exception below, which requires a concretely quoted misreading).
  - Bad: `誤解しないでほしいのだが、〜を否定したいわけではない。`
- Do not declare the body's own character or scope, even in negative or short form; a negative form or short
  length does not exempt a sentence whose topic is the text itself.
  - Bad: `テクニックの列挙はしない。`
  - Bad: `〜の話ではない。問いは〜だけである。`
- Do not narrate this norm's own devices as an operation about to happen; realize the device through content
  instead of announcing it.
  - Bad: `先に答えを半分だけ置く。`
  - Bad: `最後にもう一度だけ線を引く。`
- Do not preview a section's progress at the section's end; the driving force between sections belongs at
  the head of the next section as a counter-question or discomfort, not as a tail-end preview.
  - Bad: `ここまでで〜は見えた。次の問いは〜である。`
  - Bad: `次は〜を見る。`

Padding disguised as punchline: padding appears not only as long explanatory prose but also as short
assertions. Reshaping a document-updating sentence into a short, punchy declarative form instead of deleting
it makes it look like a well-paced punchline, and this is the largest route by which padding survives
editing. Brevity and good rhythm are not, by themselves, a reason to keep a sentence — judge a beat's quality
only for sentences that have already passed the topic test.

Sentences that update the situation or the narrator's state of judgment are good looseness and should be
kept:

- Good: `うまくいっているに違いない。` (a preconception later betrayed by fact, a setup for a later reversal)
- Good: `まあ、今すぐ手を打つほどでもないのだけど、どこかの時点で整理は要るだろう。` (updates the state of a
  suspended judgment)
- Good: `最初からわかっていたらそうしていたのに、というのが口惜しい。` (visualizes the emotion produced by a
  margin of misjudgment)
- Good: `そうしたかった、とは思う。` (a concession to a counter-question, footing for the turn that follows)

Even sentences that describe the document may be kept, but only in the following four forms:

1. **Rebuttal handling** — quote the reader's likely misreading or objection concretely in 「」 and dispel
   it.
   - Example: 「ここまでの話を『〜せよ』という主張と読まれると、それは違う」
   - Condition: the dispelled target must be concretely quoted; a vague 「誤解しないでほしい」 alone is
     padding.
2. **Question setup and recovery** — at a boundary, place a question (only after tension has already been
   created) and later its recovery. Only the question sentence itself and its recovery sentence may be kept.
   - Example: 「この章では〜を考える」 ... 「その答えの半分がこれである」
   - A declaration of what the body "is not" or "does not do" (「〜の列挙はしない」「〜の話ではない」) is not
     a question-setup; delete it unless it takes the concretely-quoted-misreading form of exception 1.
3. **Reader request or disclaimer** — at a boundary, a request to the reader.
   - Example: どうか〜と割り切って読んでほしい
4. **Example frame open/close** — a sentence that opens a fictional example or scene's frame, and a sentence
   that closes it. This reminds the reader the example is fictional and returns from abstract argument to
   the scene; place it at a boundary (a section head).
   - Example: 開く: 「〜としよう」 / 閉じる: 「冒頭の例にオチを付けておこう」
   - Even when the topic looks like the text itself, if the sentence is operating the example's frame, it is
     not padding.

Deletion and rewrite procedure:

1. Upon finding a sentence that only updates the document, first delete it and read the surrounding text; if
   it still connects, that is the end.
2. If deletion breaks the logic, rewrite what the sentence was pointing at into a situation-side sentence.
   - Example: 「ここまでだと概念の説明に見える」→「この三つの性質は、どれも冒頭の失敗の中にそろっている」
3. If the rewritten sentence still makes the body itself the topic (merely shortened or reworded), the
   rewrite has failed. Unless it fits one of exceptions 1-3, delete the sentence entirely and re-bridge the
   surrounding text.

## Post-draft check procedure

After finishing a draft, run these five checks in order, mechanically.

1. **Topic test** — pick up every paragraph-initial sentence and every independent short sentence, and judge
   each: does it update the situation, or the document? Delete or rewrite document-side sentences unless
   they fit exception 3 (reader request or disclaimer). Sentences newly written during revision, or short
   sentences re-split from longer ones, are a padding-entry route, so run this test on them immediately after
   writing.
2. **Leak test** — search whether this norm's own vocabulary or example phrases (「答えの半分」「緊張」
   「回収」「線を引く」「問いを〜返す」, etc.) appear verbatim in the body. If found, that is evidence a
   device was declared rather than realized — delete the sentence and re-realize the device on the content
   side. Also confirm no section ends with a 「次は〜」-type progress preview.
3. **Tension ledger** — enumerate every question, preconception, and promise raised in the body (e.g.
   「答えは半分ずつ返す」), and point, line by line, to where each is recovered. For anything that cannot be
   pointed to, either add the recovery or delete the question.
4. **Beat check** — find spots where three or more long assertive sentences run in a row, and insert a short
   foothold, a stop, or a hesitation.
5. **Boundary check** — confirm no second-person address, request, or reticence appears mid-body; if found,
   move it to a boundary or delete it.

## Diagnostics

When diagnosing flat prose, derive the prescription from the symptom.

| Symptom | Prescription |
|---|---|
| 全段落が同じ調子で疲れる | There is no sentence beat. Apply post-draft check step 3 (tension ledger). |
| 正しいのに読み進める気がしない | There is no unresolved tension. Insert one of the opening-design tension-creating types, and use the tension ledger (step 3) to confirm at least one tension stays open from that point on. |
| 理論の節で急に温度が下がる | Theory appears before the discomfort that should precede it. Place a counter-question or confession before the theory, and land each enumerated item on a scene one at a time (enumeration landing). |
| 緩い文はあるのに弛んで見える | The looseness has become document-updating (progress-narration). Run the topic test and rewrite it into a situation-side sentence — a slight emotional nuance, a suspended judgment, a preconception. |
| 章末が説教くさい | It closes while still in abstraction. Put a sentence up front that lands on a concrete the reader already has, and end leaving one question undecided (question resolution and closing). |
| 冒頭が事務的 | It has become an attitude-less agenda table. Rather than deleting it, give the preview sentence an attitude, or place the handling of the reader's felt sense and resistance before the preview. |

## Concepts

- **Cognitive mode switch** — the four alternating reader modes this skill designs for: observation
  (観察), hesitation (逡巡), assertion (断定), re-observation (再観察). Pacing is the deliberate switching
  between these, not a change in information density.
- **Unresolved tension** — an unanswered question, an unverified conviction, or a promised-later answer
  that the text keeps open. At least one must remain open at all times; once all tension closes, the reader
  has no reason to keep reading.
- **Situation vs. document** — the single axis for judging whether a sentence is looseness or padding: does
  it update the situation (the depicted world or the narrator's judgment state) or the document (how the
  text itself looks, or what comes next)? See Looseness vs. padding above.
- **Sentence beat** — the plant-flow-stop rhythm of short/long/short sentences, and the alternation of
  assertion and hesitation, that keeps a paragraph from reading as monotone.
- **Density wave** — the alternation of dense and sparse paragraphs, and of concrete-hugging and
  one-level-back paragraphs, that keeps a section from reading as uniformly heavy.

## When to apply

- **Drafting new prose** — is this new Japanese explanatory or narrative prose meant to read as compelling,
  not merely correct? If yes, apply technical-writing's japanese/prose_norms first, then this skill's
  opening design, section entry, sentence rhythm, paragraph density wave, enumeration landing, and question
  resolution/closing categories. If no — purely reference-style documentation — use technical-writing's
  prose_norms alone.
- **Diagnosing flat prose** — does the draft feel technically correct but tiring or unengaging to read? If
  yes, match the symptom against the Diagnostics table above and apply the paired prescription. If the draft
  instead has correctness or hygiene issues, use technical-writing's prose_norms categories.
- **Post-draft mechanical check** — has a full draft or revision pass just been completed? If yes, run the
  five-step post-draft check procedure before treating the draft as done; otherwise keep drafting, since the
  check requires a complete draft to be meaningful.

## Related

- [technical-writing](../technical-writing/SKILL.md) — apply its japanese/prose_norms ruleset first, for
  sentence-level correctness, argument rigor, and hygiene, before applying this skill's pacing and tension
  norms.
