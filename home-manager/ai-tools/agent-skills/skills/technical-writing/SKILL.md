---
name: technical-writing
description: Use when writing a blog post, technical article, tutorial, report, PR/issue body, or doc/comment prose, in English or Japanese (includes general prose mechanics, a Japanese prose-quality ruleset, and a long-form structure ruleset for books and serials).
metadata:
  version: "3.2.0"
---

Structured patterns for writing technical blogs, articles, and tutorials that communicate technical concepts
to external audiences, in English or Japanese. Pick the article type that matches the reader's need below,
apply the language-specific conventions, and for book chapters or serials layer the long-form structure
ruleset on top. The general prose mechanics below apply to any human-facing writing, not only the article
types; reach for them alone when revising a report, a PR/issue body, or documentation prose.

## General prose mechanics

Classify each passage before writing it, and do not mix the two in one passage:

- **Procedural**: tells the reader what to do; imperative mood, one instruction per sentence.
- **Descriptive**: explains what a thing is or does; no imperative, one new point per sentence.

Put the condition before the command ("If the build fails, read the log"; a trailing condition gets dropped
by a skimming reader). Describe an action with a verb, not a nominalization ("compress the file", not
"perform compression"; in Japanese, over-nominalizing with a サ変名詞 chain like「保守性担保機能の形骸化の
防止」erases who does what, so write it as an action instead:「この機能を使うと、保守性を保てる」). A
requirement is "must"; a capability is "can"; do not write "should" in
instructions: readers treat it as optional. Never convert genuine uncertainty into assertion: a hedge that
carries real epistemic content (an unverified fact, an inference, a reader's likely doubt) keeps its
uncertainty, and is deleted only once the text grounds the claim. Delete filler: announcements ("In this
section..."), empty intensifiers ("robust", "comprehensive"), padding connectives, by removing it, not by
rephrasing around it.

The Japanese-specific rules under Language guidelines extend this same rigor (argument structure, redundancy,
LLM-tell avoidance) with additional detail; apply them together when writing Japanese. To test whether any of
this prose actually reads clearly to its intended audience once written, see [cold-read](../cold-read/SKILL.md).

## Article types

### Tutorial

Use when teaching readers how to accomplish a specific task step by step. For understanding rather than
doing, use concept explanation; for choosing between options, use comparison. Audience: developers learning a
new skill.

Structure: Problem statement / what you'll learn → Prerequisites → Step-by-step instructions → Complete
working example → Troubleshooting common issues → Next steps / further reading.

### Concept explanation

Use when explaining a complex concept for deeper understanding. Audience: developers seeking understanding.

Structure: Hook / why this matters → Core concept explanation → Analogies and visualizations → Practical
examples → Common misconceptions → When to use / when to avoid.

### Comparison

Use when helping readers choose between multiple options. Audience: developers making technical decisions.

Structure: Context and criteria → Overview of each option → Feature-by-feature comparison → Benchmark results
(if applicable) → Use case recommendations → Conclusion with clear guidance.

### Case study

Use when sharing a real-world implementation experience. Audience: developers and technical leaders.

Structure: Background and challenge → Solution approach → Implementation details → Results and metrics →
Lessons learned → Recommendations.

### Opinion piece

Audience: experienced developers.

Structure: Thesis statement → Supporting arguments with evidence → Counterarguments addressed → Practical
implications → Call to action.

## Writing principles

- **Hook early**: capture attention in the first paragraph: a problem the reader faces, a surprising fact,
  or a brief relatable story. Never open with "In this article...".
- **Inverted pyramid**: lead with the conclusion; most important information first, details follow. Provide
  a TL;DR for long articles.
- **One idea per section**: if a section covers multiple ideas, split it; use headings that summarize the
  key point.
- **Show, don't tell**: demonstrate with working code, diagrams, and before/after comparisons rather than
  abstract description.
- **Credibility**: cite sources and benchmarks, acknowledge limitations, show your work. Test all code
  examples before publishing and verify technical claims against sources.

## Title patterns

- **How-to**: How to [achieve result] with [tool/technique], e.g. "How to Implement Rate Limiting with
  Redis"
- **Number list**: [N] [Things] Every Developer Should Know About [Topic], e.g. "5 Things Every Developer
  Should Know About TypeScript Generics"
- **Comparison**: [A] vs [B]: Which Should You Choose in [Year]?, e.g. "REST vs GraphQL: Which Should You
  Choose in 2026?"
- **Problem/solution**: Solving [Problem] with [Solution], e.g. "Solving N+1 Queries with DataLoader"
- **Deep dive**: Understanding [Concept]: A Deep Dive, e.g. "Understanding React Reconciliation: A Deep
  Dive"
- **Lessons**: What I Learned [Building/Using] [Thing], e.g. "What I Learned Building a Real-Time
  Collaboration System"

## Language guidelines

### English

Conversational but professional; confident, helpful, peer-to-peer tone. Use first person ("I found that...")
or second person ("You can..."). Vary sentence length between short and medium. Avoid overly formal academic
language and buzzwords without substance.

### Japanese

- Style: 読者との対話を意識した文体
- Tone: 専門的だが親しみやすい
- Formality: 技術記事: です・ます調、個人ブログ: 柔軟に
- Avoid: 過度に硬い表現、主語の省略による曖昧さ
- Note: 英語の技術用語は適度にカタカナで使用可

Domain calibration runs on an axis independent of the article types above (which classify by reader need)
and of Formality (register): it classifies by document domain instead. A technical article favors
restoring concrete steps and holding bullets to the 15% cap below, with a kanji ratio around 20% (open the
rest to hiragana or verbs). A business document (PR body, spec, report) bans metaphor outright and names
the responsible party instead of a passive construction (say「開発チームで合意した」, not「合意に至った」),
and maps a condition to its result one-to-one (say「エラーコード404の場合は3回まで再試行する」, not「適宜
対応する」). An essay or personal piece keeps the writer's own subject and plain sensory detail instead of
generalizing to「一般に」「多くの人が」, and does not inflate a small experience into a lesson (say「ここの
コーヒーは酸味が少なくて飲みやすかった」, not a grandiose「日常の喧騒という虚飾を剥ぎ取った先に現れる静謐な
真実」). A business document has no counterpart among the article types above; a technical article and an
essay loosely overlap with the Tutorial/Concept group and Opinion piece respectively, but the two axes are
not interchangeable.

The ruleset below is the canonical set for drafting and revising Japanese technical prose. Directive text is
English; illustrative bad/good examples and Japanese-specific tokens (中黒「・」, dashes「—」「―」「――」, 「」,
footnote [^label], phrases) are kept in Japanese verbatim because they demonstrate Japanese-punctuation and
particle norms.

#### Formatting

- Write one sentence per line; separate paragraphs with a blank line.
- Put code, diff, log, and config fragments in code blocks.
- Demote side notes (term origins, names of formalizations) to footnotes [^ラベル] instead of inlining them.
- Bullet lists are fine for definitions and classifications; bold the term being defined. Bold a term on its
  first definition or introduction; for later mentions, quotation, or nicknames use 「」.
- Do not use dashes (em dash「—」, horizontal bar「―」, double dash「――」) in Japanese body text or headings. For
  appositive or parenthetical insertion (「A——挿入——B」) use parentheses（）; for restatement (「A——B」) split into
  two sentences with 。 or join with 、. Exception (not covered): en dash「–」for ranges, English compounds
  like Curry–Howard, code blocks, and bibliographic info.
- Do not use 中黒（・）for Japanese parallel or enumeration. It is allowed only inside a single proper noun.
- Do not cram two elements into a heading or column heading with a rule line (罫線「─」U+2500 or dashes) like
  「種別──主題」「主題──概念」. A heading is a single natural phrase (narrow to one element, or join with a
  particle or comma). Column headings must not be bare category names like「基礎」「補足」; specify content,
  e.g.「同値関係としての分類」「ループ不変条件と帰納法」.
- A bullet listing a term and its definition uses a fullwidth colon, not a rule line:「**用語**：説明」.
- Do not end a sentence, a heading, or the line introducing a bullet list with a fullwidth colon（：）. Close
  with 。, or state the content directly without the colon-led preamble.
  - Bad: 対応方針は以下の通りです：
  - Good: 対応方針は次の3点である。
- Do not insert half-width spaces around an English word or alphabetic string embedded in Japanese text
  (e.g.「この README は」). Join them directly:「このREADMEは」.
- Delete a parenthetical aside that only restates its host phrase in different words and adds no new
  information (e.g.「AI生成文（素の出力）」→「AI生成文」;「不自然な日本語（いわゆるAI臭さ）」→「AI特有の
  不自然な日本語」). Keep a parenthetical only when it adds a fact the host phrase does not already carry.

#### Rhythm

- Keep the average sentence length around 30-45 characters. Split a sentence past 60 characters into two;
  conversely, do not stack several sentences under 10 characters in a row for dramatic effect.
- Keep commas (読点) to 0-2 per sentence. Do not open a sentence with an emphatic comma that has no
  syntactic function (「前提を、〜」「ここでは、〜」).
- Open a run of dense kanji compounds (漢語の連続) by replacing one with a native-Japanese word or a verb
  where that keeps the meaning, instead of leaving three or more kanji compounds chained in one clause.
- Do not let the same sentence ending (「です」「ます」「でした」「である」) repeat three or more times in a
  row; vary it with a different ending form, a 体言止め, or a different verb conjugation. This governs
  flowing paragraph prose; it does not apply to a status line, a field label, or code.
  - Bad: 設定を変更した。再起動した。ログを確認した。
  - Good: 設定を変更し、再起動する。その後、ログを確認した。

#### Paragraph and argument

- Use paragraph writing (パラグラフライティング). A paragraph is one step of argument; the reader must follow
  the logic paragraph by paragraph.
- One topic per paragraph. Split long paragraphs that mix multiple scene-progressions (investigation, report,
  verification, evaluation) into one-step paragraphs.
- The first sentence of a paragraph should reveal what the paragraph is about.
- Make the relation between paragraphs clear; add a connective only when the relation would otherwise be
  ambiguous.
- Lead with the conclusion when it helps the reader, then give grounds and address objections without
  repeating the conclusion mechanically.
- Defenses of an example (looks contrived, pre-empting) must not break the flow mid-climax; handle them
  together at the start of the next section.
- Explicitly deny a likely wrong reading before stating the real reason (「その理由は『〜だから』ではない。〜だからだ」).
- When negating with「AではなくB」, add one sentence of grounds for the negation; a counterfactual (「もしAなら、〜だっただろう」) is often usable.
- In concession (「確かに〜」), stay at fact confirmation. Do not assert, as the author's voice or as
  causation, something you later correct (self-contradiction). To provisionally grant a surface diagnosis,
  attribute it to the reader or common view (「〜と要約できてしまうかもしれない」).
- Do not pre-reveal climax information (figures, specific facts) in the paragraph before the climax.
- When negating or limiting, quote the exact proposition being negated in 「」 (e.g.「明文化されていればすべてを任せられる」を意味しない); do not settle for vague negation like「何もかもが解決するわけではない」.
- Place forward references (「後の章で扱う」) where the argument has settled (paragraph or section end), not
  mid-argument.

#### Argument rigor

- Do not mechanically convert speculation, possibility, reader-doubt, or counterfactual into assertion.
  「かもしれない」「だろう」「ようだ」「らしい」are removed only when they weaken a claim without grounds; keep
  the uncertainty when they express unconfirmed possibility, a character's perception, log-based inference, a
  doubt the reader might hold, or a counterfactual. Convert to assertion only when the proposition is settled
  by in-text grounds.
  - Bad: 提示し続けているかもしれない → 提示し続けている
  - Good: 提示し続けている可能性がある
- Do not lump distinct things as "the same".
  - Bad: 三つの互いに依存する未決定の決定を「同じ決定を別々に下していた」と書く
  - Good: どれも別々の決定であり、しかも互いに依存している
- Do not reduce a multi-factor event to a single cause; if an example contains multiple kinds of problems,
  separate them and map which tool explains which.
  - Bad: 契約の欠如と情報隠蔽の失敗が混在した事象を一括して「情報隠蔽の問題」と説明する
- Keep treatment of the same concept consistent across chapters and sections (do not classify something as
  「人間が決める」in one section and「チームで合意する」in another). Classification, definition, and term
  status must be uniform throughout.
- When asserting causation, state the mechanism in one sentence; do not write「AだとBになる」and omit the
  reason.
  - Bad: 手順で分けると変更が全体に波及する
  - Good: 各工程がデータを受け渡すための表現を共有してしまい、その表現を変えると全体に波及する
- Do not write detection, guarantee, or resolution as if always achievable; state conditionally and precisely
  (「〜しやすい」「〜できることが多い」「〜が成り立つときに限り」).
- Verify the given example actually supports the whole claim; if it supports only part, narrow the claim to
  match the example.
- A point deferred forward with「次節で扱う」must actually be paid off there; do not plant foreshadowing you
  never collect.
- After a concession or limitation (「ただし」「とはいえ」), always advance the argument; do not end on the
  adversative and leave it hanging.
- A section's central term must have its definition or scope stated before use within or before that
  section; do not start using it undefined.
- When merging several concepts under one umbrella term, state in one sentence just before naming that they
  reduce to the same thing; also bridge the reverse (decomposition) operation.

#### Reader load

- Omit decorative names, but preserve file names, identifiers, and other locators needed to verify a claim,
  even when they appear only once.
- When an abstract phrase's referent is not uniquely determined by context, pin it down in place with a
  parenthetical apposition so the reader need not look back.
- When adding a new example or scene increases the context the reader must hold, preface it with what
  differs from the prior example and why another is needed.
- In chapter and section intros, do not pack excessive detail unrelated to the example to come.
- Even within an example section, omit only the detail unrelated to that section's question or consequence;
  keep concretes needed for the argument. Typical omissions: decorative precision of agent reports
  (timestamps, HTTP status, coverage %), and proper nouns not referenced later.

#### Perspective and voice

- In examples, write an actor-as-subject chain of actions (「リポジトリを調査して特定し、見つけてくれた」),
  not a list of results or passive voice (「特定され、判明した」).
- Do not give an inanimate subject (a concept, tool, architecture, or event) agency or intent it cannot
  have. Rewrite it as a human or organizational action, or an observable fact.
  - Bad: この事例が残したのは、先例である。
  - Good: この事例は、先例として参照できる。
  - Bad: アーキテクチャが開発者に規律を要求する。
  - Good: 開発者はアーキテクチャの規則に従ってコードを書く。
  A result-reporting subject stays fine (「結果が〜を示す」); the ban is on treating a non-agent as if it
  acted with intent.
- Separate the reader's action from a system's capability: a request to the reader is「〜してください」; a
  capability or behavior being described is「〜できます」「〜が可能です」. Do not blur the two with a bare
  「〜します」that leaves the actor ambiguous.
- Do not gratuitously prefix fictional personas like「入社2年目のエンジニアが」.
- In argument, do not call the reader「あなた」; use a role name (「開発者」「読者」). Reserve second-person
  address for limited spots (scene setup「〜としよう」, chapter or book closings).
- Choose concrete referents; do not blur with broad words like「AI」「ツール」.
- Once you introduce a formalization or term (K, 契約, 不変条件, etc.), keep using that word; do not regress
  to vague words like「文脈」「ツール」「AI」(using「文脈」as a pre-formalization introductory word is fine).
- Choose the conventional term in the field for a translation or technical term (push notification is
  「配信」not「配送」); do not assign a near-synonym Kanji word by general feel.
- Refer to people themselves in the original spelling (Lehman, Bainbridge). But for historical figures or
  eponymous concepts introduced by their settled name, use the katakana nickname current in Japanese.
- Do not repurpose a term-sounding word into a non-term context (calling the chain from system to human a
  「経路」). Use ordinary phrasing like「届くまでの流れ」「あいだに何があるか」.

#### Restraint

This is restraint, not a total ban; use rhetoric only where it works.

- Use suspense (「ここには〜が潜んでいる」) or rhetorical questions to dramatize a derivation only where the
  tension aids the argument; where explanation suffices, just state it.
- Do not overuse the device of isolating a short punchline into its own paragraph for tension. A short
  体言止め within a paragraph (「ここまでわずか数十秒。」) is allowed only at a climax.
- Do not overuse bold in body text; limit to logical crux points (misreading-preventing negations, section
  conclusions): roughly 1-2 bolded spans per 1,000 characters of body text, not per section, so the limit
  scales with length. Otherwise let sentence order and structure do the emphasizing.
- Keep bulleted lines to at most 15% of an article's total lines; reserve bullets for genuinely parallel
  data (parameter lists, comparison tables) and write everything else, including a line of reasoning, as
  paragraph prose.
- Prefer the worker-judgment form (「〜するわけにはいかない」) over the imperative assertion
  (「〜してはならない」).
- Do not over-dramatize turning points; one factual sentence usually suffices. Only at an argument's climax,
  a short sentence with an exclamation mark is tolerable.
- Do not stoke fear of accidents or danger by enumerating consequences.
- Do not pre-announce a claim with「重要なのは〜である」; just state the claim. (A preface declaring the
  claim's form, like「標語として言い換えれば」, is fine.)
- Do not overuse the antithetical punchline「AではなくBだった」. Light supplements or evaluations may be added
  in parentheses.
- Do not use twisted idioms (「知識を体に入れる」) or metaphors whose referent is not uniquely determined
  (「報告の外側に世界が広がっている」); say it plainly with simple verbs (「身につく」「気付く機会が減る」).

#### LLM-tell avoidance

High value; after drafting, self-check against this. Using the book's own terms in argument is fine; the
problem is empty decoration. Japanese phrase tokens are kept verbatim.

- Avoid announcement and summary padding:「重要なのは〜である」「本章では〜を扱う／探求する」「ここでは〜について見ていく」「まとめると」「要するに」(when only restating the prior line),「〜に他ならない」.
- Avoid the「正面から」family:「正面から扱う」「正面から回収する」「正面から見る／書く／立てる」; they declare
  stance instead of content.
- Avoid empty adjectives:「不可欠」「核心的」「鍵となる」「根本的な」(emphasis without explaining the claim),
  「多角的」「包括的」「総合的」(without saying what was examined how).
- Avoid empty verbs:「掘り下げる」「深掘りする」「言語化する」(ends without showing what was written how),
  「触れる」「言及する」(one-paragraph brush-off).
- Avoid connective templates:「〜において」「〜という側面から」「〜の観点から」(no new info),「さらに」「また」
  「加えて」in a row.
- Avoid weak hedges and praise:「〜と言えるだろう」「〜かもしれない」(only when weakening a claim groundlessly;
  keep for speculation, hypothesis, reader-doubt, or character-perception),「非常に」「極めて」「大いに」
  (empty intensifiers).
- Avoid AI's favorite metaphor verbs that paper over a mechanism with imagery:「効く」「効いてくる」(say
  「役に立つ」「〜を防げる」),「壊れる」(say「要件を満たせなくなる」「例外が発生する」「整合性が崩れる」),
  「倒す」(say「〜を原則とする」「除外する」),「溶かす」(say「〜に時間を費やした」),「潰す」(say「解消する」
  「検証する」),「踏み込む」(say「詳細まで調べる」),「引き返す」(say「元の状態に戻す」「ロールバックする」),
  「添える」(say「参照する」「付与する」),「収斂する」(say「〜に決まる」「〜に落ち着く」),「沈黙する」「黙って」
  (say「エラーを出さずに」「通知なく」). The replacement must name the actual mechanism, not swap one vague
  word for another.
- Avoid the incident-vocabulary spike these same verbs feed:「事故」「混ざる」「落とし穴」「破綻」「実害」
  「素通り」for a failure (say what broke and how);「実測」「疑う」「照合」「突き合わせる」「断定」
  「取り違える」for checking (say what was measured or compared);「入口」「土台」「道具」「主役」
  「構図」「線引き」for role or position (name the actual role; 「核心」stays fine when it grounds a specific
  claim, as in the Self-check example below, so it is not listed here as a bare filler);「既定」「別物」「定番」「要点」「定石」
  「桁違い」as filler evaluation (state the specific difference or default instead).
- Avoid grandiose vocabulary imported to dramatize an ordinary observation or experience:「真理」「虚飾」
  「境地」「美学」「深淵」「冷徹」「禁欲的」「優美」「極致」「宿命」, and pseudo-concrete jargon that sounds
  technical but adds nothing checkable:「手触り」「肌感」「温度感」「熱量」(describe the actual steps, logs,
  or measured values instead),「解像度」as in「解像度を上げる」(say what was investigated and how far),
  「腹落ち」「メンタルモデル」(「文脈」as a pre-formalization word stays fine per the perspective-and-voice
  rule above),「本質」「地に足のついた」「等身大」, and an invented compound term standing in for a real
  technical name (e.g.「意思決定OS」; name the actual mechanism or use the standard term).
- Avoid direct-translation leaks from English idiom beyond the em dash and antithesis rules above:「〜した
  瞬間」for "the moment ..." (say「〜するとすぐに」「〜した直後に」),「〜を正本にする」for "single source of
  truth" (say「信頼できる唯一の情報源とする」),「〜を指している」「〜を示唆している」used as a hedge for
  "point to / suggest" (say「〜を表している」「〜と考えられる」when the claim is actually grounded),「耐力
  のある」for "load-bearing" (say「根幹となる」「外せない」; do not say「不可欠な」, which the empty-adjective
  bullet above already bans).

Self-check examples:

- Bad: 本章では、型理論を正面から扱う
- Good: 型理論は、式を型によって分類する
- Bad: 多角的に分析すると、重要なのは〜である
- Good: 評価の核心は、正しさを誰が知っているかにある

#### Redundancy

- Do not restate the same claim in paraphrase; write each claim once.
- If adjacent sections state the same thing from different angles, their roles overlap; absorb one into the
  other into a single section.
- Do not re-summarize a scene right after depicting it; leave only a single interpretive sentence
  (「このような作業は、ほぼ完全に任せられる」).
- Combine parallel facts with the same logical role into one sentence; mark their logical status with the
  lead word (「当然、経理部の月次処理も顧客の支払いも〜」).
- Do not write intermediate steps the reader can supply.
- If a multi-sentence argument compresses to one sentence, keep only the one;「要するに」is allowed as a
  compression signal.
- Do not add sentences that exist only for connection or evaluation (「それ自体はよいことである」).
- Do not use imagined reader Q&A as rhetoric (posing a question and answering in one word); state the claim
  directly. Also avoid acting out the reader's reaction (「〜と感じたかもしれない。そのとおりである」); make
  concessions plainly in the body (「もちろん、処置そのものは開発者が決める問題ではない」).
- Do not frame a likely reader idea with meta scaffolding (「ここまでの話には自然な続きがある」「〜という発想である」); write the idea directly. A reader's question may stay as a question (「その保守も任せればよいのではないだろうか」).
- Do not write author-stance disclaimers (「本書もそれを否定しない」); state only the fact (「〜に書かせる場合が多い」).
- Make prose share context with the reader in the fewest steps; if it lands without unrolling every step,
  name the structure and assert it.
- Do not preemptively bring out concepts or document names not yet introduced in the body.
- Match the predicate to the evidence. 「有効」does not imply「必須」: necessity needs its own grounds.
  Preserve genuine uncertainty, possibility, hypothesis, and reader-doubt.
- Connectives that set rhythm (「しかし一方で」) are not counted as redundancy.

#### Headings

- Make headings specific enough to identify content: the question the section answers, or the object it
  treats.
- Do not use procedure-only headings (「例に戻す」「〜を読み直す」) or information-free headings.
- A heading may state the conclusion when that helps navigation; avoid a theatrical punchline that hides
  what the section actually covers.
- A noun phrase naming the section's object is acceptable.
- Whether interrogative or declarative does not matter; what matters is that it points to the object or the
  reader's question. Choose whichever suits the body's tone.

#### Honesty to reader

- If an example may look contrived, do not hide it; pre-empt the reader's doubt and add brief grounds that it
  is realistically plausible.
- Ground plausibility in a cited observation or explain the example's assumptions. Do not replace an
  unsupported assertion with an invented appeal to common experience.
- Do not smoothly write unconfirmed things as if confirmed.

### Bilingual

Adapt style to each language's conventions rather than translating literally. Keep technical terms
consistent. Adjust examples for cultural relevance when appropriate.

## Long-form structure

Patterns for book chapters, multi-part series, and other long-form pieces where the reader progresses across
many sections. These operate above the single-article patterns above and are language-neutral; when writing
Japanese, also apply the Japanese prose-norms ruleset above. They govern how a chapter opens, how examples
grow, and how a chapter or a whole work closes.

### Chapter arc

Shape each chapter or installment as problem-then-solution:

1. **Introduction**: make the conventional approach's failure concrete before naming the new approach. Show
   the scenario where the old method breaks (a realistic "works on my machine" or "the build fails on the new
   OS" situation) so the solution is felt as relief rather than asserted as superior.
2. **Foundation**: the core structure or theory of the new approach.
3. **Migration**: how to move from the old approach to the new one, shown as a side-by-side change of one
   concrete example.
4. **Practice**: a worked, realistic example.
5. **Deep dive**: the mechanism behind why it works.
6. **Outlook**: a bridge to the next chapter's higher demand.

Introduce the solution in three graded moves: what visibly changes ("it becomes this"), why it changes (the
mechanism), then the essential value ("so, in effect"). Build credibility with one concrete, checkable fact
(adoption status, a count of supported items, who authored it) rather than adjectives. Keep the introduction
focused on the problem and prerequisites needed for the example; do not expand it to meet a word count.

### Code example escalation

Grow code examples in stages rather than presenting one large block:

1. **Minimal example**: the smallest version that runs.
2. **Incremental complexity**: add one axis at a time (an environment variable, then a hook, then a
   service), each as its own step.
3. **Integrated example**: combine the pieces into one realistic configuration.

Split code at meaningful boundaries when that improves comprehension, while preserving enough context to
run or understand it. Explain the purpose and non-obvious result where the surrounding prose does not
already do so. Do not add narration to meet a prose-to-code ratio. Trim log output to the lines needed for
the claim, retaining errors and other evidence that qualifies the result.

### Concluding chapter rubric

Include a closing section when it adds synthesis or a next decision. Useful elements include:

- **Recap**: name the specific technical elements the chapter taught, not a vague gesture at "what we
  covered".
- **Significance**: state what was achieved and which problem it solved.
- **Next steps**: point to concrete next actions and the resources needed for them.

Chapter endings come in two types: **bridging**, which hands off to the next chapter's demand ("the
environment built here now faces a higher requirement"), the default for interior chapters, and
**terminal**, which ends a series by integrating all prior chapters; it must still name the individual
results, not only assert a "culmination".

A closing section has no word or paragraph quota. Checklist: does it connect the relevant results rather
than merely repeat the title? Is
there a single message focus rather than several competing ones? Is the ending type (bridging vs terminal)
consistent with sibling chapters' endings?

### Long-form redundancy trimming

Reduce the bloat that accumulates across a long piece:

- Merge duplicated messages: the same claim restated in three places (a feature's "everything is automated"
  line appearing across three consecutive subsections) collapses to one.
- Prune redundant links, but retain distinct sources needed to support the claims or the next action.
- Cut encouragement padding ("surely", "you should be able to ...") and re-summaries that immediately follow
  the thing they summarize.
- Define a central term before or at its first use within a section, and keep its meaning stable across
  chapters instead of reintroducing it with a shifted sense.

## Related

- [cold-read](../cold-read/SKILL.md): dispatch a context-free reviewer to test whether the written prose
  actually reads clearly to its audience
- [serena-usage](../serena-usage/SKILL.md): symbol operations for extracting code examples from projects
- [investigation-patterns](../investigation-patterns/SKILL.md): researching technical topics and verifying
  claims
- [technical-documentation](../technical-documentation/SKILL.md): creating reference documentation from blog
  content
