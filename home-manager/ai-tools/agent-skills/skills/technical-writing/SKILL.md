---
name: Technical Writing
description: This skill should be used when the user asks to "write blog post", "technical article", "tutorial", "explain concept", or needs guidance on technical writing for external audiences. Provides patterns for technical blogs and articles in both English and Japanese. Includes a Japanese prose-quality ruleset for drafting and revising Japanese manuscripts, plus a language-neutral long-form structure ruleset for books and serialized pieces.
version: 2.2.0
---

<purpose>
  Provide structured patterns for writing technical blogs and articles that effectively communicate technical concepts to external audiences.
</purpose>

<tools>
  <tool>Write</tool>
  <tool>Read</tool>
  <tool>Edit</tool>
  <tool>WebSearch</tool>
  <tool>mcp__plugin_claude-code-home-manager_context7__resolve-library-id</tool>
  <tool>mcp__plugin_claude-code-home-manager_context7__query-docs</tool>
</tools>

<concepts>
  <concept name="article_types">Five types: tutorial (task-based), concept explanation (deep understanding), comparison (decision help), case study (real-world story), opinion piece (best practices)</concept>
  <concept name="hook_principle">Capture attention in the first paragraph using problem statement, surprising fact, or relatable story</concept>
  <concept name="inverted_pyramid">Lead with conclusion, most important information first, details follow</concept>
  <concept name="show_dont_tell">Demonstrate with working code, diagrams, and before/after comparisons rather than abstract description</concept>
</concepts>

<patterns>
  <pattern name="tutorial">
    <description>Step-by-step guide to accomplish a specific task</description>
    <decision_tree name="when_to_use">
      <question>Are you teaching readers how to accomplish a specific task?</question>
      <if_yes>Create tutorial with step-by-step instructions and working examples</if_yes>
      <if_no>Consider concept explanation for understanding or comparison for decision-making</if_no>
    </decision_tree>
    <audience>Developers learning a new skill</audience>
    <structure>
      <section>Problem statement / What you'll learn</section>
      <section>Prerequisites</section>
      <section>Step-by-step instructions</section>
      <section>Complete working example</section>
      <section>Troubleshooting common issues</section>
      <section>Next steps / Further reading</section>
    </structure>
    <length>1500-3000 words</length>
  </pattern>

  <pattern name="concept_explanation">
    <description>Deep dive into a technical concept</description>
    <decision_tree name="when_to_use">
      <question>Are you explaining a complex concept for deeper understanding?</question>
      <if_yes>Create concept explanation with examples and misconceptions</if_yes>
      <if_no>Use tutorial for task-based learning or opinion piece for best practices</if_no>
    </decision_tree>
    <audience>Developers seeking understanding</audience>
    <structure>
      <section>Hook / Why this matters</section>
      <section>Core concept explanation</section>
      <section>Analogies and visualizations</section>
      <section>Practical examples</section>
      <section>Common misconceptions</section>
      <section>When to use / When to avoid</section>
    </structure>
    <length>1000-2500 words</length>
  </pattern>

  <pattern name="comparison">
    <description>Compare technologies, approaches, or tools</description>
    <decision_tree name="when_to_use">
      <question>Are you helping readers choose between multiple options?</question>
      <if_yes>Create comparison with feature analysis and use case recommendations</if_yes>
      <if_no>Use tutorial for implementation or concept explanation for understanding</if_no>
    </decision_tree>
    <audience>Developers making technical decisions</audience>
    <structure>
      <section>Context and criteria</section>
      <section>Overview of each option</section>
      <section>Feature-by-feature comparison</section>
      <section>Benchmark results (if applicable)</section>
      <section>Use case recommendations</section>
      <section>Conclusion with clear guidance</section>
    </structure>
    <length>1500-2500 words</length>
  </pattern>

  <pattern name="case_study">
    <description>Real-world implementation story</description>
    <decision_tree name="when_to_use">
      <question>Are you sharing a real-world implementation experience?</question>
      <if_yes>Create case study with challenge, solution, and lessons learned</if_yes>
      <if_no>Use tutorial for general how-to or opinion piece for best practices</if_no>
    </decision_tree>
    <audience>Developers and technical leaders</audience>
    <structure>
      <section>Background and challenge</section>
      <section>Solution approach</section>
      <section>Implementation details</section>
      <section>Results and metrics</section>
      <section>Lessons learned</section>
      <section>Recommendations</section>
    </structure>
    <length>1500-3000 words</length>
  </pattern>

  <pattern name="opinion_piece">
    <description>Technical opinion or best practices</description>
    <audience>Experienced developers</audience>
    <structure>
      <section>Thesis statement</section>
      <section>Supporting arguments with evidence</section>
      <section>Counterarguments addressed</section>
      <section>Practical implications</section>
      <section>Call to action</section>
    </structure>
    <length>800-1500 words</length>
  </pattern>
</patterns>

<writing_principles>
  <principle name="hook_early">
    <description>Capture attention in the first paragraph</description>
    <technique>Start with a problem the reader faces</technique>
    <technique>Use a surprising fact or statistic</technique>
    <technique>Tell a brief relatable story</technique>
  </principle>

  <principle name="one_idea_per_section">
    <description>Each section should have a single clear purpose</description>
    <guideline>If a section covers multiple ideas, split it</guideline>
    <guideline>Use headings that summarize the key point</guideline>
  </principle>

  <principle name="show_dont_tell">
    <description>Demonstrate concepts with examples</description>
    <technique>Include working code snippets</technique>
    <technique>Use diagrams for architecture</technique>
    <technique>Show before/after comparisons</technique>
  </principle>

  <principle name="respect_reader_time">
    <description>Be concise, get to the point</description>
    <technique>Lead with the conclusion</technique>
    <technique>Use bullet points for lists</technique>
    <technique>Provide TL;DR for long articles</technique>
  </principle>

  <principle name="credibility">
    <description>Build trust through accuracy and honesty</description>
    <technique>Cite sources and benchmarks</technique>
    <technique>Acknowledge limitations</technique>
    <technique>Show your work (methodology)</technique>
  </principle>
</writing_principles>

<language_guidelines>
  <english>
    <style>Conversational but professional</style>
    <tone>Confident, helpful, peer-to-peer</tone>
    <voice>First person ("I found that...") or second person ("You can...")</voice>
    <sentence_length>Vary between short and medium</sentence_length>
    <avoid>Overly formal academic language, buzzwords without substance</avoid>
  </english>

  <japanese>
    <style>読者との対話を意識した文体</style>
    <tone>専門的だが親しみやすい</tone>
    <formality>技術記事: です・ます調、個人ブログ: 柔軟に</formality>
    <avoid>過度に硬い表現、主語の省略による曖昧さ</avoid>
    <note>英語の技術用語は適度にカタカナで使用可</note>
    <prose_norms>
      <description>Canonical ruleset for drafting and revising Japanese technical prose. Directive text is English; illustrative bad/good examples and Japanese-specific tokens (中黒「・」, dashes「—」「―」「――」, 「」, footnote [^label], phrases) are kept in Japanese verbatim because they demonstrate Japanese-punctuation and particle norms.</description>
      <category name="formatting">
        <rule>Write one sentence per line; separate paragraphs with a blank line.</rule>
        <rule>Put code, diff, log, and config fragments in code blocks.</rule>
        <rule>Demote side notes (term origins, names of formalizations) to footnotes [^ラベル] instead of inlining them.</rule>
        <rule>Bullet lists are fine for definitions and classifications; bold the term being defined. Bold a term on its first definition or introduction; for later mentions, quotation, or nicknames use 「」.</rule>
        <rule>Do not use dashes (em dash「—」, horizontal bar「―」, double dash「――」) in Japanese body text or headings. For appositive or parenthetical insertion (「A——挿入——B」) use parentheses（）; for restatement (「A——B」) split into two sentences with 。 or join with 、. Exception (not covered): en dash「–」for ranges, English compounds like Curry–Howard, code blocks, and bibliographic info.</rule>
        <rule>Do not use 中黒（・）for Japanese parallel or enumeration. It is allowed only inside a single proper noun.</rule>
        <rule>Do not cram two elements into a heading or column heading with a rule line (罫線「─」U+2500 or dashes) like「種別──主題」「主題──概念」. A heading is a single natural phrase (narrow to one element, or join with a particle or comma). Column headings must not be bare category names like「基礎」「補足」; specify content, e.g.「同値関係としての分類」「ループ不変条件と帰納法」.</rule>
        <rule>A bullet listing a term and its definition uses a fullwidth colon, not a rule line:「**用語**：説明」.</rule>
      </category>
      <category name="paragraph_and_argument">
        <rule>Use paragraph writing (パラグラフライティング). A paragraph is one step of argument; the reader must follow the logic paragraph by paragraph.</rule>
        <rule>One topic per paragraph. Split long paragraphs that mix multiple scene-progressions (investigation, report, verification, evaluation) into one-step paragraphs.</rule>
        <rule>The first sentence of a paragraph should reveal what the paragraph is about.</rule>
        <rule>At a paragraph's start, make the logical relation to the previous paragraph explicit with a connective (「であれば」「実際」「しかし」「この例自体からも」).</rule>
        <rule>Advance the argument in one direction. Do not use a "conclude then handle objection then restate conclusion" shape. Finish objections and doubts first, then state the conclusion once.</rule>
        <rule>Defenses of an example (looks contrived, pre-empting) must not break the flow mid-climax; handle them together at the start of the next section.</rule>
        <rule>Explicitly deny a likely wrong reading before stating the real reason (「その理由は『〜だから』ではない。〜だからだ」).</rule>
        <rule>When negating with「AではなくB」, add one sentence of grounds for the negation; a counterfactual (「もしAなら、〜だっただろう」) is often usable.</rule>
        <rule>In concession (「確かに〜」), stay at fact confirmation. Do not assert, as the author's voice or as causation, something you later correct (self-contradiction). To provisionally grant a surface diagnosis, attribute it to the reader or common view (「〜と要約できてしまうかもしれない」).</rule>
        <rule>Do not pre-reveal climax information (figures, specific facts) in the paragraph before the climax.</rule>
        <rule>When negating or limiting, quote the exact proposition being negated in 「」 (e.g.「明文化されていればすべてを任せられる」を意味しない); do not settle for vague negation like「何もかもが解決するわけではない」.</rule>
        <rule>Place forward references (「後の章で扱う」) where the argument has settled (paragraph or section end), not mid-argument.</rule>
      </category>
      <category name="argument_rigor">
        <rule>Do not mechanically convert speculation, possibility, reader-doubt, or counterfactual into assertion.「かもしれない」「だろう」「ようだ」「らしい」are removed only when they weaken a claim without grounds; keep the uncertainty when they express unconfirmed possibility, a character's perception, log-based inference, a doubt the reader might hold, or a counterfactual. Convert to assertion only when the proposition is settled by in-text grounds.
          <bad>提示し続けているかもしれない → 提示し続けている</bad>
          <good>提示し続けている可能性がある</good>
        </rule>
        <rule>Do not lump distinct things as "the same".
          <bad>三つの互いに依存する未決定の決定を「同じ決定を別々に下していた」と書く</bad>
          <good>どれも別々の決定であり、しかも互いに依存している</good>
        </rule>
        <rule>Do not reduce a multi-factor event to a single cause; if an example contains multiple kinds of problems, separate them and map which tool explains which.
          <bad>契約の欠如と情報隠蔽の失敗が混在した事象を一括して「情報隠蔽の問題」と説明する</bad>
        </rule>
        <rule>Keep treatment of the same concept consistent across chapters and sections (do not classify something as「人間が決める」in one section and「チームで合意する」in another). Classification, definition, and term status must be uniform throughout.</rule>
        <rule>When asserting causation, state the mechanism in one sentence; do not write「AだとBになる」and omit the reason.
          <bad>手順で分けると変更が全体に波及する</bad>
          <good>各工程がデータを受け渡すための表現を共有してしまい、その表現を変えると全体に波及する</good>
        </rule>
        <rule>Do not write detection, guarantee, or resolution as if always achievable; state conditionally and precisely (「〜しやすい」「〜できることが多い」「〜が成り立つときに限り」).</rule>
        <rule>Verify the given example actually supports the whole claim; if it supports only part, narrow the claim to match the example.</rule>
        <rule>A point deferred forward with「次節で扱う」must actually be paid off there; do not plant foreshadowing you never collect.</rule>
        <rule>After a concession or limitation (「ただし」「とはいえ」), always advance the argument; do not end on the adversative and leave it hanging.</rule>
        <rule>A section's central term must have its definition or scope stated before use within or before that section; do not start using it undefined.</rule>
        <rule>When merging several concepts under one umbrella term, state in one sentence just before naming that they reduce to the same thing; also bridge the reverse (decomposition) operation.</rule>
      </category>
      <category name="reader_load">
        <rule>Do not introduce proper nouns (file names, function names, identifiers) that are not referenced later; use general phrasing like「仕様書」「金額計算のユーティリティ」.</rule>
        <rule>When an abstract phrase's referent is not uniquely determined by context, pin it down in place with a parenthetical apposition so the reader need not look back.</rule>
        <rule>When adding a new example or scene increases the context the reader must hold, preface it with what differs from the prior example and why another is needed.</rule>
        <rule>In chapter and section intros, do not pack excessive detail unrelated to the example to come.</rule>
        <rule>Even within an example section, omit only the detail unrelated to that section's question or consequence; keep concretes needed for the argument. Typical omissions: decorative precision of agent reports (timestamps, HTTP status, coverage %), and proper nouns not referenced later.</rule>
      </category>
      <category name="perspective_and_voice">
        <rule>In examples, write an actor-as-subject chain of actions (「リポジトリを調査して特定し、見つけてくれた」), not a list of results or passive voice (「特定され、判明した」).</rule>
        <rule>Do not gratuitously prefix fictional personas like「入社2年目のエンジニアが」.</rule>
        <rule>In argument, do not call the reader「あなた」; use a role name (「開発者」「読者」). Reserve second-person address for limited spots (scene setup「〜としよう」, chapter or book closings).</rule>
        <rule>Choose concrete referents; do not blur with broad words like「AI」「ツール」.</rule>
        <rule>Once you introduce a formalization or term (K, 契約, 不変条件, etc.), keep using that word; do not regress to vague words like「文脈」「ツール」「AI」(using「文脈」as a pre-formalization introductory word is fine).</rule>
        <rule>Choose the conventional term in the field for a translation or technical term (push notification is「配信」not「配送」); do not assign a near-synonym Kanji word by general feel.</rule>
        <rule>Refer to people themselves in the original spelling (Lehman, Bainbridge). But for historical figures or eponymous concepts introduced by their settled name, use the katakana nickname current in Japanese.</rule>
        <rule>Do not repurpose a term-sounding word into a non-term context (calling the chain from system to human a「経路」). Use ordinary phrasing like「届くまでの流れ」「あいだに何があるか」.</rule>
      </category>
      <category name="restraint">
        <note>This is restraint, not a total ban; use rhetoric only where it works.</note>
        <rule>Use suspense (「ここには〜が潜んでいる」) or rhetorical questions to dramatize a derivation only where the tension aids the argument; where explanation suffices, just state it.</rule>
        <rule>Do not overuse the device of isolating a short punchline into its own paragraph for tension. A short 体言止め within a paragraph (「ここまでわずか数十秒。」) is allowed only at a climax.</rule>
        <rule>Do not overuse bold in body text; limit to logical crux points (misreading-preventing negations, section conclusions), one or two per section, intro allowed. Otherwise let sentence order and structure do the emphasizing.</rule>
        <rule>Prefer the worker-judgment form (「〜するわけにはいかない」) over the imperative assertion (「〜してはならない」).</rule>
        <rule>Do not over-dramatize turning points; one factual sentence usually suffices. Only at an argument's climax, a short sentence with an exclamation mark is tolerable.</rule>
        <rule>Do not stoke fear of accidents or danger by enumerating consequences.</rule>
        <rule>Do not pre-announce a claim with「重要なのは〜である」; just state the claim. (A preface declaring the claim's form, like「標語として言い換えれば」, is fine.)</rule>
        <rule>Do not overuse the antithetical punchline「AではなくBだった」. Light supplements or evaluations may be added in parentheses.</rule>
        <rule>Do not use twisted idioms (「知識を体に入れる」) or metaphors whose referent is not uniquely determined (「報告の外側に世界が広がっている」); say it plainly with simple verbs (「身につく」「気付く機会が減る」).</rule>
      </category>
      <category name="llm_tell_avoidance">
        <note>High value; after drafting, self-check against this. Using the book's own terms in argument is fine; the problem is empty decoration. Japanese phrase tokens are kept verbatim.</note>
        <rule>Avoid announcement and summary padding:「重要なのは〜である」「本章では〜を扱う／探求する」「ここでは〜について見ていく」「まとめると」「要するに」(when only restating the prior line),「〜に他ならない」.</rule>
        <rule>Avoid the「正面から」family:「正面から扱う」「正面から回収する」「正面から見る／書く／立てる」— they declare stance instead of content.</rule>
        <rule>Avoid empty adjectives:「不可欠」「核心的」「鍵となる」「根本的な」(emphasis without explaining the claim),「多角的」「包括的」「総合的」(without saying what was examined how).</rule>
        <rule>Avoid empty verbs:「掘り下げる」「深掘りする」「言語化する」(ends without showing what was written how),「触れる」「言及する」(one-paragraph brush-off).</rule>
        <rule>Avoid connective templates:「〜において」「〜という側面から」「〜の観点から」(no new info),「さらに」「また」「加えて」in a row.</rule>
        <rule>Avoid weak hedges and praise:「〜と言えるだろう」「〜かもしれない」(only when weakening a claim groundlessly; keep for speculation, hypothesis, reader-doubt, or character-perception),「非常に」「極めて」「大いに」(empty intensifiers).</rule>
        <rule>Self-check examples:
          <bad>本章では、〇〇の理論を正面から扱う</bad>
          <good>本章では、〇〇の理論を扱う</good>
          <bad>多角的に分析すると、重要なのは〜である</bad>
          <good>評価の核心は、正しさを誰が知っているかにある</good>
        </rule>
      </category>
      <category name="redundancy">
        <rule>Do not restate the same claim in paraphrase; write each claim once.</rule>
        <rule>If adjacent sections state the same thing from different angles, their roles overlap; absorb one into the other into a single section.</rule>
        <rule>Do not re-summarize a scene right after depicting it; leave only a single interpretive sentence (「このような作業は、ほぼ完全に任せられる」).</rule>
        <rule>Combine parallel facts with the same logical role into one sentence; mark their logical status with the lead word (「当然、経理部の月次処理も顧客の支払いも〜」).</rule>
        <rule>Do not write intermediate steps the reader can supply.</rule>
        <rule>If a multi-sentence argument compresses to one sentence, keep only the one;「要するに」is allowed as a compression signal.</rule>
        <rule>Do not add sentences that exist only for connection or evaluation (「それ自体はよいことである」).</rule>
        <rule>Do not use imagined reader Q&amp;A as rhetoric (posing a question and answering in one word); state the claim directly. Also avoid acting out the reader's reaction (「〜と感じたかもしれない。そのとおりである」); make concessions plainly in the body (「もちろん、処置そのものは開発者が決める問題ではない」).</rule>
        <rule>Do not frame a likely reader idea with meta scaffolding (「ここまでの話には自然な続きがある」「〜という発想である」); write the idea directly. A reader's question may stay as a question (「その保守も任せればよいのではないだろうか」).</rule>
        <rule>Do not write author-stance disclaimers (「本書もそれを否定しない」); state only the fact (「〜に書かせる場合が多い」).</rule>
        <rule>Make prose share context with the reader in the fewest steps; if it lands without unrolling every step, name the structure and assert it.</rule>
        <rule>Do not preemptively bring out concepts or document names not yet introduced in the body.</rule>
        <rule>Do not settle for weak hesitant predicates (「有効な対策であり」); state what is settled by in-text grounds strongly and concretely (「活用において必須であり」). Keep weak predicates that express genuine uncertainty, possibility, hypothesis, or reader-doubt; intentional softening for tone (「必須だと言ってもいい」) is allowed.</rule>
        <rule>Connectives that set rhythm (「しかし一方で」) are not counted as redundancy.</rule>
      </category>
      <category name="headings">
        <rule>Make headings specific enough to identify content: the question the section answers, or the object it treats.</rule>
        <rule>Do not use procedure-only headings (「例に戻す」「〜を読み直す」) or information-free headings.</rule>
        <rule>Do not make a heading a "punchline" that states the section's conclusion (avoid spoiling the payoff at the heading).</rule>
        <rule>A noun phrase naming the section's object is acceptable.</rule>
        <rule>Whether interrogative or declarative does not matter; what matters is that it points to the object or the reader's question. Choose whichever suits the body's tone.</rule>
      </category>
      <category name="honesty_to_reader">
        <rule>If an example may look contrived, do not hide it; pre-empt the reader's doubt and add brief grounds that it is realistically plausible.</rule>
        <rule>Ground it not in the author's assertion (「十分あり得る状況だ」) but in a general fact or common view the reader can recognize from experience (「この症状は珍しくないだろう」「〜という言い方もよく耳にする」).</rule>
        <rule>Do not smoothly write unconfirmed things as if confirmed.</rule>
      </category>
    </prose_norms>
  </japanese>

  <bilingual>
    <rule>Adapt style to each language's conventions (not literal translation)</rule>
    <rule>Keep technical terms consistent</rule>
    <rule>Adjust examples for cultural relevance when appropriate</rule>
  </bilingual>
</language_guidelines>

<output>
  <format>
    <article_plan>
      - Type: [tutorial/concept/comparison/case_study/opinion]
      - Audience: [beginner/intermediate/advanced developers]
      - Core message: [one sentence]
      - Language: [en/ja/both]
      - Target length: [word count]</article_plan>
    <outline>[Section headings with key points]</outline>
    <draft>[Article content]</draft>
    <edit_checklist>
      - [ ] Hook is compelling
      - [ ] Each section has one clear purpose
      - [ ] Code examples tested and working
      - [ ] Technical claims verified
      - [ ] Read aloud for flow
      - [ ] Trimmed unnecessary words
      - [ ] Title and headings optimized</edit_checklist>
  </format>
</output>

<title_patterns>
  <pattern name="how_to">
    <description>How to [achieve result] with [tool/technique]</description>
    <example>How to Implement Rate Limiting with Redis</example>
  </pattern>
  <pattern name="number_list">
    <description>[N] [Things] Every Developer Should Know About [Topic]</description>
    <example>5 Things Every Developer Should Know About TypeScript Generics</example>
  </pattern>
  <pattern name="comparison">
    <description>[A] vs [B]: Which Should You Choose in [Year]?</description>
    <example>REST vs GraphQL: Which Should You Choose in [Current Year]?</example>
  </pattern>
  <pattern name="problem_solution">
    <description>Solving [Problem] with [Solution]</description>
    <example>Solving N+1 Queries with DataLoader</example>
  </pattern>
  <pattern name="deep_dive">
    <description>Understanding [Concept]: A Deep Dive</description>
    <example>Understanding React Reconciliation: A Deep Dive</example>
  </pattern>
  <pattern name="lessons">
    <description>What I Learned [Building/Using] [Thing]</description>
    <example>What I Learned Building a Real-Time Collaboration System</example>
  </pattern>
</title_patterns>

<long_form_structure>
  <description>Patterns for book chapters, multi-part series, and other long-form pieces where the reader progresses across many sections. These operate above the single-article patterns and are language-neutral; when writing Japanese, also apply the japanese/prose_norms ruleset. They govern how a chapter opens, how examples grow, and how a chapter or a whole work closes.</description>

  <pattern name="chapter_arc">
    <description>Shape each chapter or installment as problem-then-solution</description>
    <structure>
      <section>Introduction: make the conventional approach's failure concrete before naming the new approach. Show the scenario where the old method breaks (a realistic "works on my machine" or "the build fails on the new OS" situation) so the solution is felt as relief rather than asserted as superior.</section>
      <section>Foundation: the core structure or theory of the new approach.</section>
      <section>Migration: how to move from the old approach to the new one, shown as a side-by-side change of one concrete example.</section>
      <section>Practice: a worked, realistic example.</section>
      <section>Deep dive: the mechanism behind why it works.</section>
      <section>Outlook: a bridge to the next chapter's higher demand.</section>
    </structure>
    <technique>Introduce the solution in three graded moves: what visibly changes ("it becomes this"), why it changes (the mechanism), then the essential value ("so, in effect"). Build credibility with one concrete, checkable fact (adoption status, a count of supported items, who authored it) rather than adjectives.</technique>
    <length>Keep the introduction's problem framing tight (roughly 300-400 words); a longer intro delays the payoff and front-loads detail unrelated to the example to come.</length>
  </pattern>

  <pattern name="code_example_escalation">
    <description>Grow code examples in stages rather than presenting one large block</description>
    <sequence>
      <stage>Minimal example: the smallest version that runs.</stage>
      <stage>Incremental complexity: add one axis at a time (an environment variable, then a hook, then a service), each as its own step.</stage>
      <stage>Integrated example: combine the pieces into one realistic configuration.</stage>
    </sequence>
    <guideline>Cap a single code block at what a reader can absorb in a few minutes (on the order of 50-70 lines); split anything longer.</guideline>
    <guideline>Frame each block: one or two sentences of lead-in before it, a two- or three-sentence takeaway after it, and defer the deeper "why" to the following subsection.</guideline>
    <guideline>Watch the prose-to-code balance. A section that is mostly code with thin narration is under-explained; when code exceeds roughly two-thirds of a section, add explanation or move some code to a later stage.</guideline>
    <guideline>Trim decorative shell or log output to the few lines that carry the point (a 40-line status dump where 5-10 lines matter is noise).</guideline>
    <guideline>Do not let example difficulty regress: the last example in a chapter should not be the simplest one.</guideline>
  </pattern>

  <pattern name="concluding_chapter_rubric">
    <description>Evaluate and write closing sections (chapter endings and the final chapter)</description>
    <required_beats>
      <beat name="recap">Name the specific technical elements the chapter taught, not a vague gesture at "what we covered".</beat>
      <beat name="significance">State what was achieved and which problem it solved.</beat>
      <beat name="next_steps">Point to concrete next actions and, at most, the single most important learning resource.</beat>
    </required_beats>
    <chapter_ending_types>
      <type name="bridging">Ends by handing off to the next chapter's demand ("the environment built here now faces a higher requirement"). The default for interior chapters.</type>
      <type name="terminal">Ends a series by integrating all prior chapters. It must still name the individual results, not only assert a "culmination".</type>
    </chapter_ending_types>
    <length>A closing section is short (about half a page, ~600 words across 3-4 paragraphs). Under-length is as much a defect as over-length: a final chapter that claims to integrate everything but omits the individual results is incomplete.</length>
    <checklist>
      <item>Does it reference the chapter's actual sections and results, not just its title?</item>
      <item>Is there a single message focus rather than several competing ones?</item>
      <item>Is the ending type (bridging vs terminal) consistent with sibling chapters' endings?</item>
    </checklist>
  </pattern>

  <pattern name="long_form_redundancy_trimming">
    <description>Reduce the bloat that accumulates across a long piece</description>
    <technique>Merge duplicated messages: the same claim restated in three places (a feature's "everything is automated" line appearing across three consecutive subsections) collapses to one.</technique>
    <technique>Prune URL enumeration: cite the single most important link, not a list.</technique>
    <technique>Cut encouragement padding ("surely", "you should be able to ...") and re-summaries that immediately follow the thing they summarize.</technique>
    <technique>Define a central term before or at its first use within a section, and keep its meaning stable across chapters instead of reintroducing it with a shifted sense.</technique>
  </pattern>
</long_form_structure>

<best_practices>
  <practice priority="critical">
    <description>Start with a compelling hook in the first paragraph</description>
    <technique>Use a problem the reader faces, a surprising fact, or a brief relatable story</technique>
  </practice>
  <practice priority="critical">
    <description>Put the most important point first using inverted pyramid structure</description>
    <technique>Lead with the conclusion, provide TL;DR for long articles</technique>
  </practice>
  <practice priority="critical">
    <description>Test all code examples before publishing</description>
    <technique>Verify code compiles, runs, and produces expected output</technique>
  </practice>
  <practice priority="high">
    <description>Explain code context before or after each snippet</description>
    <technique>Describe what the code does and why it's written that way</technique>
  </practice>
  <practice priority="high">
    <description>Support claims with evidence</description>
    <technique>Include benchmarks, examples, or reasoned arguments for technical claims</technique>
  </practice>
  <practice priority="high">
    <description>Each section should have one clear purpose</description>
    <technique>Split sections covering multiple ideas, use headings that summarize the key point</technique>
  </practice>
  <practice priority="medium">
    <description>Use diagrams for architectural concepts</description>
    <technique>Include visual representations to complement text explanations</technique>
  </practice>
  <practice priority="medium">
    <description>Vary sentence length for readability</description>
    <technique>Mix short and medium sentences to maintain reader engagement</technique>
  </practice>
  <practice priority="medium">
    <description>Adapt style to language conventions</description>
    <technique>Use conversational but professional tone in English, です・ます調 in Japanese technical articles</technique>
  </practice>
  <practice priority="medium">
    <description>For long-form pieces, frame the conventional approach's failure before introducing the solution, and escalate code examples from minimal to integrated</description>
    <technique>See long_form_structure: chapter_arc and code_example_escalation</technique>
  </practice>
</best_practices>

<anti_patterns>
  <avoid name="burying_the_lede">
    <description>Hiding the main point deep in the article</description>
    <instead>Put the most important point first, use inverted pyramid structure</instead>
  </avoid>
  <avoid name="assuming_motivation">
    <description>Not explaining why the topic matters</description>
    <instead>Explain why the reader should care early in the article</instead>
  </avoid>
  <avoid name="code_without_context">
    <description>Showing code without explaining its purpose</description>
    <instead>Explain what code does and why before or after the snippet</instead>
  </avoid>
  <avoid name="unsubstantiated_claims">
    <description>Making claims like "X is the best" without evidence</description>
    <instead>Support claims with benchmarks, examples, or reasoned arguments</instead>
  </avoid>
  <avoid name="clickbait_disappointment">
    <description>Title promises more than content delivers</description>
    <instead>Ensure title accurately reflects content and scope</instead>
  </avoid>
  <avoid name="wall_of_code">
    <description>Long code blocks without explanation</description>
    <instead>Break up code with explanations, highlight key lines, add comments</instead>
  </avoid>
  <avoid name="generic_introductions">
    <description>Starting with "In this article..." or similar phrases</description>
    <instead>Start with a hook that captures attention immediately</instead>
  </avoid>
</anti_patterns>

<rules priority="critical">
  <rule>Test all code examples compile and run before publishing</rule>
  <rule>Verify all technical claims with sources or benchmarks</rule>
  <rule>Start with a compelling hook, never generic openings like "In this article..."</rule>
</rules>

<rules priority="standard">
  <rule>Explain code context before or after each snippet</rule>
  <rule>Use inverted pyramid: most important point first</rule>
  <rule>Break long code blocks with explanations</rule>
  <rule>Ensure title accurately reflects content scope</rule>
</rules>

<workflow>
  <phase name="planning">
    <step order="1">Identify article type, target audience, and core message</step>
    <step order="2">Research topic using Context7 and WebSearch for accuracy</step>
    <step order="3">Create outline following the appropriate pattern structure</step>
  </phase>
  <phase name="drafting">
    <step order="1">Write the hook and introduction first</step>
    <step order="2">Draft each section following one-idea-per-section principle</step>
    <step order="3">Add code examples, diagrams, and supporting evidence</step>
  </phase>
  <phase name="revision">
    <step order="1">Verify all code examples compile and run</step>
    <step order="2">Check technical claims against sources</step>
    <step order="3">Review against edit checklist</step>
    <step order="4">Read aloud for flow and trim unnecessary words</step>
  </phase>
</workflow>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Grammar or style issue</example>
    <example severity="medium">Technical inaccuracy in example</example>
    <example severity="high">Misleading or incorrect technical claim</example>
    <example severity="critical">Content could cause harm if followed</example>
  </examples>
</error_escalation>

<constraints>
  <must>Verify all technical claims</must>
  <must>Test all code examples</must>
  <must>Write for target audience level</must>
  <avoid>Jargon without explanation</avoid>
  <avoid>Untested code examples</avoid>
  <avoid>Overly complex explanations</avoid>
</constraints>

<related_skills>
  <skill name="serena-usage">Symbol operations for extracting code examples from projects</skill>
  <skill name="context7-usage">Library documentation lookup for accurate technical references</skill>
  <skill name="investigation-patterns">Researching technical topics and verifying claims</skill>
  <skill name="technical-documentation">Creating reference documentation from blog content</skill>
</related_skills>

<related_agents>
  <agent name="docs">Primary agent for technical article and blog post generation</agent>
  <agent name="quality-assurance">Review technical writing for clarity, accuracy, and structure</agent>
</related_agents>
