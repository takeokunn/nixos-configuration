# Long-form structure

Patterns for book chapters, multi-part series, and other long-form pieces where the reader progresses across
many sections. They build on `references/article-types.md` and `references/prose-and-argument.md`, are
language-neutral, and govern how a chapter opens, how examples grow, and how a chapter or a whole work closes.

## Chapter arc

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

## Code example escalation

Grow code examples in stages rather than presenting one large block:

1. **Minimal example**: the smallest version that runs.
2. **Incremental complexity**: add one axis at a time (an environment variable, then a hook, then a
   service), each as its own step.
3. **Integrated example**: combine the pieces into one realistic configuration.

Split code at meaningful boundaries when that improves comprehension, while preserving enough context to
run or understand it. Explain the purpose and non-obvious result where the surrounding prose does not
already do so. Do not add narration to meet a prose-to-code ratio. Trim log output to the lines needed for
the claim, retaining errors and other evidence that qualifies the result.

## Concluding chapter rubric

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
than merely repeat the title? Is there a single message focus rather than several competing ones? Is the
ending type (bridging vs terminal) consistent with sibling chapters' endings?

## Links across a long piece

Prune redundant links, but retain distinct sources needed to support the claims or the next action. For a
claim restated across sections and for re-summaries, see the prose rules of the explanatory-strict output style.
