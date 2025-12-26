---
name: Technical Writing
description: This skill should be used when the user asks to "write blog post", "technical article", "tutorial", "explain concept", or needs guidance on technical writing for external audiences. Provides patterns for technical blogs and articles in both English and Japanese.
version: 0.1.0
---

<purpose>
Provide structured patterns for writing technical blogs and articles that effectively communicate technical concepts to external audiences.
</purpose>

<article_types>
<type name="tutorial">
<description>Step-by-step guide to accomplish a specific task</description>
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
</type>

<type name="concept_explanation">
<description>Deep dive into a technical concept</description>
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
</type>

<type name="comparison">
<description>Compare technologies, approaches, or tools</description>
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
</type>

<type name="case_study">
<description>Real-world implementation story</description>
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
</type>

<type name="opinion_piece">
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
</type>
</article_types>

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
</japanese>

<bilingual>
<rule>Adapt style to each language's conventions (not literal translation)</rule>
<rule>Keep technical terms consistent</rule>
<rule>Adjust examples for cultural relevance when appropriate</rule>
</bilingual>
</language_guidelines>

<workflow>
<phase name="ideation">
<step>Identify the core message (one sentence)</step>
<step>Define target audience and their current knowledge</step>
<step>Choose article type</step>
<step>Research existing content on the topic</step>
</phase>

<phase name="outline">
<step>Draft section headings</step>
<step>Note key points for each section</step>
<step>Identify examples and code snippets needed</step>
<step>Estimate length</step>
</phase>

<phase name="draft">
<step>Write the body first (not the introduction)</step>
<step>Include all code examples and test them</step>
<step>Add transitions between sections</step>
<step>Write introduction and conclusion last</step>
</phase>

<phase name="edit">
<step>Read aloud for flow</step>
<step>Cut unnecessary words (aim for 20% reduction)</step>
<step>Verify all technical claims</step>
<step>Check code examples compile/run</step>
<step>Add/improve diagrams if helpful</step>
</phase>

<phase name="polish">
<step>Proofread for typos and grammar</step>
<step>Optimize title and headings for clarity</step>
<step>Add meta description for SEO</step>
<step>Prepare social media summary</step>
</phase>
</workflow>

<output>
<format>
## Article Plan

- Type: [tutorial/concept/comparison/case_study/opinion]
- Audience: [beginner/intermediate/advanced developers]
- Core message: [one sentence]
- Language: [en/ja/both]
- Target length: [word count]

## Outline

[Section headings with key points]

## Draft

[Article content]

## Edit Checklist

- [ ] Hook is compelling
- [ ] Each section has one clear purpose
- [ ] Code examples tested and working
- [ ] Technical claims verified
- [ ] Read aloud for flow
- [ ] Trimmed unnecessary words
- [ ] Title and headings optimized
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
<example>REST vs GraphQL: Which Should You Choose in 2025?</example>
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
