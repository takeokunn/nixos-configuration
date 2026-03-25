---
name: technical-writing
description: "Use when asked to write a blog post, technical article, tutorial, concept explanation, or when guidance on technical writing for external audiences is needed. Provides patterns for technical blogs and articles in English and Japanese."
---

Structured patterns for writing technical blogs and articles that effectively communicate technical concepts to external audiences. The agent should start with a compelling hook, test all code examples, and match writing style to the target audience.

## Critical Rules

- Test all code examples compile and run before publishing
- Verify all technical claims with sources or benchmarks
- Start with a compelling hook — never generic openings like "In this article..."
- Use inverted pyramid: most important point first

## Workflow

1. **Plan** — Select article type, audience, core message, language, and target length
2. **Outline** — Create section headings with key points
3. **Draft** — Write article content following writing principles
4. **Edit** — Verify hook is compelling, each section has one purpose, code tested, claims verified, trimmed unnecessary words

## Article Type Selection

| Type | Audience | Length | When to Use |
|------|----------|--------|-------------|
| Tutorial | Developers learning a new skill | 1500-3000 words | Teaching how to accomplish a specific task |
| Concept Explanation | Developers seeking understanding | 1000-2500 words | Deep dive into a technical concept |
| Comparison | Developers making decisions | 1500-2500 words | Choosing between multiple options |
| Case Study | Developers and tech leaders | 1500-3000 words | Sharing real-world implementation experience |
| Opinion Piece | Experienced developers | 800-1500 words | Technical opinions or best practices |

## Article Structures

### Tutorial

1. Problem statement / What the reader will learn
2. Prerequisites
3. Step-by-step instructions with working examples
4. Complete working example
5. Troubleshooting common issues
6. Next steps / Further reading

### Concept Explanation

1. Hook / Why this matters
2. Core concept explanation
3. Analogies and visualizations
4. Practical examples
5. Common misconceptions
6. When to use / When to avoid

### Comparison

1. Context and evaluation criteria
2. Overview of each option
3. Feature-by-feature comparison (table format)
4. Benchmark results (if applicable)
5. Use case recommendations
6. Conclusion with clear guidance

## Writing Principles

### Hook Early

Capture attention in the first paragraph:
- Start with a problem the reader faces
- Use a surprising fact or statistic
- Tell a brief relatable story

### One Idea Per Section

Each section should have a single clear purpose. If a section covers multiple ideas, split it. Use headings that summarize the key point.

### Show, Don't Tell

```python
# Bad: "Our function is fast"
# Good: Show benchmark results
import time
start = time.time()
result = process_batch(10_000_items)
elapsed = time.time() - start
print(f"Processed 10K items in {elapsed:.2f}s")  # Output: Processed 10K items in 0.03s
```

### Respect Reader Time

- Lead with the conclusion
- Use bullet points for lists
- Provide TL;DR for long articles

### Build Credibility

- Cite sources and benchmarks
- Acknowledge limitations
- Show methodology

## Title Patterns

| Pattern | Example |
|---------|---------|
| How to [result] with [tool] | How to Implement Rate Limiting with Redis |
| [N] Things About [Topic] | 5 Things Every Developer Should Know About TypeScript Generics |
| [A] vs [B]: Which to Choose | REST vs GraphQL: Which Should You Choose in 2025? |
| Solving [Problem] with [Solution] | Solving N+1 Queries with DataLoader |
| Understanding [Concept]: A Deep Dive | Understanding React Reconciliation: A Deep Dive |
| What I Learned [Building/Using] [Thing] | What I Learned Building a Real-Time Collaboration System |

## Language Guidelines

### English

- Conversational but professional tone
- First person ("I found that...") or second person ("You can...")
- Vary sentence length between short and medium
- Avoid overly formal academic language and buzzwords without substance

### Japanese

- 読者との対話を意識した文体 — conversational style aware of the reader
- 技術記事: です・ます調; 個人ブログ: 柔軟に
- Avoid: 過度に硬い表現、主語の省略による曖昧さ
- English technical terms may be used in katakana as appropriate

### Bilingual

- Adapt style to each language's conventions (not literal translation)
- Keep technical terms consistent across languages
- Adjust examples for cultural relevance when appropriate

## Best Practices

- Explain code context before or after each snippet — describe what it does and why
- Support claims with benchmarks, examples, or reasoned arguments
- Use diagrams for architectural concepts
- Break long code blocks with explanations highlighting key lines
- Ensure title accurately reflects content scope

## Anti-Patterns to Avoid

- **Burying the lede** — put the most important point first
- **Assuming motivation** — explain why the topic matters early
- **Code without context** — explain purpose before or after snippets
- **Unsubstantiated claims** — support with benchmarks or evidence
- **Clickbait titles** — ensure title matches content scope
- **Wall of code** — break up with explanations
- **Generic introductions** — start with a hook, not "In this article..."

## Error Escalation

- **Low:** Grammar or style issue — fix issue, follow style guide
- **Medium:** Technical inaccuracy in example — verify and correct
- **High:** Misleading or incorrect technical claim — stop, verify before publishing
- **Critical:** Content could cause harm if followed — block publication, require expert review
