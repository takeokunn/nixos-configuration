---
name: Web UX
description: This skill should be used when the user asks to "improve UX", "usability review", "web accessibility", "design a form/onboarding flow", "information architecture", or works on web/app user experience. Also covers interaction mechanics — focus ownership across stacked modals and overlays, focus traps that survive re-rendering and late-mounted content, the open/close contract of a dialog and when refusing to close is correct, one overlay serving both a busy and an error state needing opposite assistive-technology semantics, classifying each setting as immediate-apply or review-before-commit, keyboard shortcut ownership against text entry, and releasing held input on every discontinuity such as a window blur, tab switch, or overlay opening. Provides web UX heuristics, accessibility (WCAG), and performance-perception best practices.
version: 2.3.0
---

<purpose>
  Provide a definitive, framework-agnostic collection of established web UX heuristics, accessibility standards, and performance-perception principles for evaluating and designing usable, accessible, and trustworthy interfaces.
</purpose>

<concepts>
<concept name="nn_visibility_of_status">
  <description>Nielsen-Norman heuristic 1: The system should always keep users informed about what is going on through appropriate, timely feedback</description>
  <guidance>Acknowledge every user action within a perceptible window; show progress, current location, and state changes</guidance>
  <when>Loading data, submitting forms, multi-step flows, background processing</when>
</concept>

<concept name="nn_match_real_world">
  <description>Nielsen-Norman heuristic 2: The system should speak the user's language with familiar words, phrases, and concepts rather than internal jargon</description>
  <guidance>Follow real-world conventions; present information in a natural and logical order</guidance>
  <when>Labels, terminology, icons, error messages, ordering of options</when>
</concept>

<concept name="nn_user_control_freedom">
  <description>Nielsen-Norman heuristic 3: Users need a clearly marked emergency exit to leave unwanted states without an extended dialogue</description>
  <guidance>Support undo and redo; provide cancel, back, and close affordances</guidance>
  <when>Destructive actions, wizards, modals, accidental navigation</when>
</concept>

<concept name="nn_consistency_standards">
  <description>Nielsen-Norman heuristic 4: Users should not have to wonder whether different words, situations, or actions mean the same thing; follow platform and industry conventions</description>
  <guidance>Maintain internal consistency (within product) and external consistency (with platform conventions)</guidance>
  <when>Naming, layout, interaction patterns, component behavior</when>
</concept>

<concept name="nn_error_prevention">
  <description>Nielsen-Norman heuristic 5: Even better than good error messages is a careful design that prevents problems from occurring in the first place</description>
  <guidance>Eliminate error-prone conditions or surface a confirmation before users commit to an action</guidance>
  <when>Forms, destructive operations, irreversible actions, ambiguous input</when>
</concept>

<concept name="nn_recognition_over_recall">
  <description>Nielsen-Norman heuristic 6: Minimize memory load by making elements, actions, and options visible; the user should not have to remember information across the interface</description>
  <guidance>Surface options rather than requiring recall; keep instructions visible or easily retrievable</guidance>
  <when>Navigation, search, multi-step processes, command interfaces</when>
</concept>

<concept name="nn_flexibility_efficiency">
  <description>Nielsen-Norman heuristic 7: Accelerators unseen by novices can speed up interaction for experts, letting the system serve both</description>
  <guidance>Offer shortcuts, defaults, and personalization without burdening new users</guidance>
  <when>Power-user workflows, repeated tasks, keyboard shortcuts</when>
</concept>

<concept name="nn_aesthetic_minimalist">
  <description>Nielsen-Norman heuristic 8: Interfaces should not contain information that is irrelevant or rarely needed; every extra unit competes with the relevant units</description>
  <guidance>Prioritize content and features by relevance; reduce visual noise</guidance>
  <when>Dense dashboards, content-heavy pages, feature-laden screens</when>
</concept>

<concept name="nn_error_recovery">
  <description>Nielsen-Norman heuristic 9: Error messages should be expressed in plain language, precisely indicate the problem, and constructively suggest a solution</description>
  <guidance>State what went wrong, why, and how to fix it; avoid codes without explanation</guidance>
  <when>Validation failures, system errors, failed operations</when>
</concept>

<concept name="nn_help_documentation">
  <description>Nielsen-Norman heuristic 10: It is best if the system needs no documentation, but help may be necessary; it should be easy to search, focused on the user's task, and concise</description>
  <guidance>Provide just-in-time, task-focused help; make it findable and actionable</guidance>
  <when>Complex features, first-run experiences, edge-case workflows</when>
</concept>

<concept name="law_hicks">
  <description>Hick's Law: The time to make a decision increases logarithmically with the number and complexity of choices</description>
  <guidance>Reduce or group choices, use progressive disclosure, highlight recommended options</guidance>
  <when>Menus, pricing tables, settings, onboarding decisions</when>
</concept>

<concept name="law_fitts">
  <description>Fitts's Law: The time to acquire a target is a function of the distance to and the size of the target</description>
  <guidance>Make important and frequent targets larger and closer to the pointer or thumb; use screen edges and corners</guidance>
  <when>Buttons, touch targets, primary actions, navigation</when>
</concept>

<concept name="law_miller">
  <description>Miller's Law: The average person can hold about seven (plus or minus two) items in working memory; commonly applied as a chunking principle</description>
  <guidance>Chunk content into meaningful groups rather than relying on a precise magic number</guidance>
  <when>Phone numbers, navigation groups, form sections, list grouping</when>
</concept>

<concept name="law_jakobs">
  <description>Jakob's Law: Users spend most of their time on other sites, so they prefer your site to work the same way as the others they already know</description>
  <guidance>Follow established conventions; deviate only with strong justification and clear benefit</guidance>
  <when>Navigation placement, icon meaning, interaction patterns, layout</when>
</concept>

<concept name="law_teslers">
  <description>Tesler's Law (Conservation of Complexity): Every application has an inherent amount of complexity that cannot be removed, only moved between the system and the user</description>
  <guidance>Absorb complexity into the system where possible rather than offloading it onto users</guidance>
  <when>Configuration, smart defaults, automation, input parsing</when>
</concept>

<concept name="law_doherty_threshold">
  <description>Doherty Threshold: Productivity soars when a system and its users interact at a pace (under ~400ms) that ensures neither has to wait on the other</description>
  <guidance>Keep response feedback under ~400ms; use perceived-performance techniques when actual work takes longer</guidance>
  <when>Interactions, transitions, data loading, input feedback</when>
</concept>

<concept name="law_peak_end">
  <description>Peak-End Rule: People judge an experience largely based on how they felt at its most intense point (peak) and at its end, rather than the average</description>
  <guidance>Design memorable positive peaks and a strong, satisfying ending; mitigate pain at critical moments</guidance>
  <when>Onboarding completion, checkout, error recovery, task completion</when>
</concept>

<concept name="law_serial_position">
  <description>Serial Position Effect: Users best remember the first (primacy) and last (recency) items in a series</description>
  <guidance>Place the most important items at the beginning and end of lists and navigation</guidance>
  <when>Navigation menus, lists, option ordering</when>
</concept>

<concept name="law_aesthetic_usability">
  <description>Aesthetic-Usability Effect: Users perceive aesthetically pleasing designs as more usable, and are more tolerant of minor usability problems</description>
  <guidance>Invest in visual quality, but do not let aesthetics mask genuine usability issues in testing</guidance>
  <when>First impressions, trust building, perceived quality</when>
</concept>

<concept name="law_von_restorff">
  <description>Von Restorff Effect (Isolation Effect): When multiple similar objects are present, the one that differs from the rest is most likely to be remembered</description>
  <guidance>Visually distinguish the single most important action or piece of information; avoid emphasizing too many elements</guidance>
  <when>Primary call-to-action, highlighting, promotions, key data</when>
</concept>

<concept name="law_zeigarnik">
  <description>Zeigarnik Effect: People remember uncompleted or interrupted tasks better than completed ones</description>
  <guidance>Use progress indicators and visible incomplete states to encourage task completion</guidance>
  <when>Onboarding checklists, multi-step flows, profile completion</when>
</concept>

<concept name="law_postels">
  <description>Postel's Law (Robustness Principle): Be conservative in what you do, be liberal in what you accept from others</description>
  <guidance>Accept varied, imperfect user input gracefully while producing reliable, well-formed output</guidance>
  <when>Form input parsing, dates, phone numbers, flexible search</when>
</concept>

<concept name="wcag_perceivable">
  <description>WCAG POUR principle 1: Information and UI components must be presentable to users in ways they can perceive</description>
  <guidance>Provide text alternatives, captions, sufficient contrast, and content that adapts without losing meaning</guidance>
  <scope>Text contrast 4.5:1 normal / 3:1 large text and UI components, alt text, captions, content reflow, do not rely on color alone</scope>
  <when>Images, media, color usage, contrast, responsive content</when>
</concept>

<concept name="wcag_operable">
  <description>WCAG POUR principle 2: UI components and navigation must be operable by all users and input methods</description>
  <guidance>Ensure full keyboard operability, visible focus, adequate target size, and no keyboard traps</guidance>
  <scope>Keyboard operability, visible focus indicator, target size (24x24 CSS px minimum at Level AA, SC 2.5.8; 44x44 enhanced at Level AAA, SC 2.5.5), no time-critical traps, no flashing beyond thresholds</scope>
  <when>Navigation, interactive controls, focus management, motion</when>
</concept>

<concept name="wcag_understandable">
  <description>WCAG POUR principle 3: Information and the operation of the UI must be understandable</description>
  <guidance>Use clear language, predictable behavior, consistent navigation, labels, instructions, and error identification</guidance>
  <scope>Labels and instructions, error identification and suggestion, predictable behavior, consistent help (WCAG 2.2), readable language</scope>
  <when>Forms, labels, error handling, navigation consistency</when>
</concept>

<concept name="wcag_robust">
  <description>WCAG POUR principle 4: Content must be robust enough to be interpreted reliably by a wide variety of user agents, including assistive technologies</description>
  <guidance>Use valid, semantic structure and expose name, role, and value of UI components to assistive technology</guidance>
  <scope>Semantic structure, valid markup, name/role/value exposed, status messages announced</scope>
  <when>Custom components, dynamic content, assistive technology support</when>
</concept>

<concept name="cwv_lcp">
  <description>Core Web Vital: Largest Contentful Paint measures loading performance; the largest content element should render quickly</description>
  <guidance>Target LCP 2.5 seconds or less; optimize images, server response, and render-blocking resources</guidance>
  <when>Initial page load, perceived speed, landing pages</when>
</concept>

<concept name="cwv_inp">
  <description>Core Web Vital: Interaction to Next Paint measures responsiveness across all interactions during a page visit</description>
  <guidance>Target INP 200 milliseconds or less; minimize main-thread work and long tasks</guidance>
  <when>Clicks, taps, key presses, interactive responsiveness</when>
</concept>

<concept name="cwv_cls">
  <description>Core Web Vital: Cumulative Layout Shift measures visual stability from unexpected layout movement</description>
  <guidance>Target CLS 0.1 or less; reserve space for media, ads, and dynamically injected content</guidance>
  <when>Image and ad loading, font swaps, late-injected UI</when>
</concept>

<concept name="perceived_performance">
  <description>Perceived performance is the user's subjective sense of speed, which can differ from measured performance and can be improved independently</description>
  <guidance>Use skeleton screens, optimistic UI, progressive and lazy loading, and immediate feedback to mask latency</guidance>
  <when>Network-bound operations, slow backends, large data sets</when>
</concept>

<concept name="visual_hierarchy">
  <description>Visual hierarchy guides the eye through content by order of importance using size, weight, color, contrast, spacing, and position</description>
  <guidance>Establish one clear focal point per view; use scale and contrast to signal priority</guidance>
  <when>Page layout, dashboards, landing pages, content design</when>
</concept>

<concept name="gestalt_principles">
  <description>Gestalt principles describe how people perceive grouped visual elements as unified wholes</description>
  <guidance>Apply proximity, similarity, closure, continuity, common region, and figure/ground to communicate relationships</guidance>
  <scope>Proximity (nearness implies grouping), similarity (alike elements group), closure (mind completes shapes), continuity (eye follows lines), common region (shared boundary groups), figure/ground (foreground vs background)</scope>
  <when>Grouping, layout, spacing, card design, separators</when>
</concept>

<concept name="typography_readability">
  <description>Readable typography depends on appropriate line length, line height, type scale, and contrast</description>
  <guidance>Keep line length around 45-75 characters; use comfortable line height; establish a consistent modular type scale</guidance>
  <when>Body text, articles, content-heavy interfaces</when>
</concept>

<concept name="spacing_grid">
  <description>Consistent spacing systems (commonly an 8pt grid) create rhythm, alignment, and visual coherence</description>
  <guidance>Use a consistent spacing scale; rely on white space to separate and group content</guidance>
  <when>Layout, component spacing, alignment, design systems</when>
</concept>

<concept name="information_architecture">
  <description>Information architecture organizes, structures, and labels content so users can find information and complete tasks (findability and understandability)</description>
  <guidance>Match structure to user mental models; validate with card sorting and tree testing</guidance>
  <when>Site structure, navigation, taxonomy, content organization</when>
</concept>

<concept name="forms_input">
  <description>Form design minimizes effort and error through clear structure, sensible defaults, and immediate recovery support</description>
  <guidance>Prefer single-column layout, top-aligned labels, minimal fields, inline validation, and clear input affordances</guidance>
  <when>Sign-up, checkout, settings, data entry</when>
</concept>

<concept name="feedback_system_status">
  <description>Feedback communicates the result and progress of user actions and the current state of the system</description>
  <guidance>Provide loading states, optimistic UI, progress indicators, empty states, confirmations, and undo</guidance>
  <when>Actions, transitions, asynchronous operations, edge states</when>
</concept>

<concept name="microcopy">
  <description>Microcopy is the small, functional text (labels, buttons, errors, hints) that guides users through an interface</description>
  <guidance>Be clear and specific; use a consistent voice and tone; write actionable, blame-free error messages and verb-based button labels</guidance>
  <when>Buttons, errors, empty states, tooltips, confirmations</when>
</concept>

<concept name="onboarding_empty_states">
  <description>Onboarding and empty states shape the first-run experience and guide users toward initial value</description>
  <guidance>Use progressive disclosure and just-in-time guidance; make empty states instructive rather than blank dead ends</guidance>
  <when>First use, new accounts, no-data views, feature discovery</when>
</concept>

<concept name="responsive_mobile">
  <description>Responsive and mobile design adapts layout and interaction to varied screen sizes and touch input</description>
  <guidance>Design mobile-first; ensure touch targets of at least 44x44px; respect thumb zones and reachability</guidance>
  <when>Multi-device support, touch interfaces, adaptive layouts</when>
</concept>

<concept name="trust_ethics">
  <description>Ethical UX respects user autonomy through transparency, honesty, and the absence of manipulative patterns</description>
  <guidance>Prevent errors, make consequences clear, and avoid dark patterns that exploit or deceive users</guidance>
  <when>Consent, pricing, subscriptions, cancellation, defaults</when>
</concept>

<concept name="focus_ownership">
  <description>At any moment exactly one surface owns keyboard containment; when surfaces stack, ownership transfers to the topmost rather than accumulating across all of them</description>
  <guidance>Give the topmost open surface sole ownership of Tab traversal and Escape; every surface beneath it must return from its key handler immediately while a higher one is open</guidance>
  <when>Stacked dialogs, a settings panel over a pause screen, a drawer over a modal, confirmation inside a wizard</when>
</concept>

<concept name="commit_model">
  <description>A commit model defines when a user's change takes effect: immediately on input, or on an explicit save the user can still abandon</description>
  <guidance>Choose per setting based on reversibility and cost rather than applying one model uniformly to a whole panel; whichever you choose, the user must be able to tell whether the change is saved</guidance>
  <when>Preferences, settings panels, filters, profile editing, configuration</when>
</concept>

<concept name="input_discontinuity">
  <description>A discontinuity is any event that interrupts a held interaction without producing the release event the interaction is waiting for</description>
  <guidance>Enumerate the discontinuity set explicitly and clear held state on every member of it; treat the list as a checklist rather than adding cases as bugs are reported</guidance>
  <scope>Overlay or modal opening, pause, navigation, window blur, document visibility change, pointer capture loss, focus moving into an editable field</scope>
  <when>Press-and-hold actions, drag, long-press, key repeat, continuous gestures</when>
</concept>
</concepts>

<patterns>
<pattern name="heuristic_evaluation">
  <description>Expert inspection of an interface against established usability heuristics to surface issues</description>
  <decision_tree name="when_to_use">
    <question>Do you need to find usability problems quickly without recruiting users?</question>
    <if_yes>Run a heuristic evaluation against Nielsen-Norman's 10 heuristics, ideally with multiple evaluators</if_yes>
    <if_no>Consider usability testing with real users for behavioral evidence</if_no>
  </decision_tree>
  <example>
    For each screen, inspect against the 10 heuristics:
    - Visibility of system status: is feedback timely and clear?
    - Match with real world: is language familiar and jargon-free?
    - User control and freedom: are undo and exit available?
    - Consistency and standards: do conventions hold internally and externally?
    - Error prevention: are risky actions guarded or confirmed?
    - Recognition over recall: are options visible rather than memorized?
    - Flexibility and efficiency: are accelerators available for experts?
    - Aesthetic and minimalist design: is irrelevant content removed?
    - Error recovery: are messages plain and constructive?
    - Help and documentation: is help findable and task-focused?
  </example>
  <note>Multiple independent evaluators uncover substantially more issues than a single reviewer; consolidate findings before rating severity</note>
</pattern>

<pattern name="severity_rating">
  <description>Rate identified usability issues on the established 0-4 severity scale to prioritize fixes</description>
  <decision_tree name="when_to_use">
    <question>Have you collected a list of usability issues that need prioritization?</question>
    <if_yes>Apply the 0-4 severity scale weighing frequency, impact, and persistence</if_yes>
    <if_no>First gather issues via heuristic evaluation or usability testing</if_no>
  </decision_tree>
  <example>
    0 = Not a usability problem
    1 = Cosmetic only, fix if time permits
    2 = Minor problem, low priority
    3 = Major problem, high priority to fix
    4 = Usability catastrophe, imperative to fix before release
    Weigh: how frequent, how much impact, how persistent the problem is.
  </example>
  <use_case>Prioritizing a backlog of usability findings before a release</use_case>
</pattern>

<pattern name="accessibility_audit">
  <description>Systematic evaluation of an interface against WCAG success criteria across the POUR principles</description>
  <decision_tree name="when_to_use">
    <question>Does the interface need to meet accessibility standards or serve assistive-technology users?</question>
    <if_yes>Run an accessibility audit covering Perceivable, Operable, Understandable, and Robust criteria</if_yes>
    <if_no>Still verify baseline keyboard operability and contrast as a minimum</if_no>
  </decision_tree>
  <example>
    Perceivable: text contrast 4.5:1 (3:1 large text/UI); meaningful alt text; captions; not relying on color alone; content reflows.
    Operable: fully keyboard operable; visible focus indicator; target size adequate; no keyboard traps; respects reduced motion.
    Understandable: every input has a programmatic label; errors are identified with suggestions; navigation is consistent; help is consistent.
    Robust: semantic structure; name, role, and value exposed to assistive tech; status messages are announced.
  </example>
  <use_case>Pre-release accessibility compliance check for a public web application</use_case>
</pattern>

<pattern name="form_design_review">
  <description>Evaluate a form against established conventions for structure, validation, and recovery</description>
  <decision_tree name="when_to_use">
    <question>Are users completing a form, signup, or checkout flow?</question>
    <if_yes>Review against single-column, minimal-field, inline-validation, smart-default conventions</if_yes>
    <if_no>Use the broader heuristic evaluation for non-form interactions</if_no>
  </decision_tree>
  <example>
    - Layout is single-column with top-aligned, persistent labels (not placeholder-only)
    - Only essential fields are requested; optional fields are marked
    - Validation is inline and timed to help, not punish, the user
    - Errors say what is wrong and how to fix it, near the field
    - Smart defaults, autocomplete, and correct input types reduce effort
    - Input accepts varied formats gracefully (Postel's Law)
    - The primary submit action is clear, labeled with a specific verb
  </example>
  <use_case>Reviewing a registration or checkout form before launch</use_case>
</pattern>

<pattern name="perceived_performance_review">
  <description>Assess both measured Core Web Vitals and perceived-performance techniques</description>
  <decision_tree name="when_to_use">
    <question>Does the experience feel slow, or do Core Web Vitals fall outside target thresholds?</question>
    <if_yes>Review measured vitals and apply perceived-performance techniques to close the gap</if_yes>
    <if_no>Establish baseline field metrics before optimizing</if_no>
  </decision_tree>
  <example>
    Measured: LCP 2.5s or less, INP 200ms or less, CLS 0.1 or less.
    Perceived: provide feedback under the Doherty threshold (~400ms); use skeleton screens for content loading; apply optimistic UI for reversible actions; progressively load below-the-fold content; reserve layout space to avoid shift.
  </example>
  <use_case>Improving the responsiveness of a data-heavy dashboard or feed</use_case>
</pattern>

<pattern name="information_architecture_review">
  <description>Validate that content structure and navigation match user mental models for findability</description>
  <decision_tree name="when_to_use">
    <question>Are users struggling to find content or navigate the product?</question>
    <if_yes>Run card sorting to derive groupings and tree testing to validate findability</if_yes>
    <if_no>Spot-check navigation labels and breadcrumbs against user terminology</if_no>
  </decision_tree>
  <example>
    - Card sorting: users group and label content to reveal mental models
    - Tree testing: users locate items in the proposed hierarchy to measure findability
    - Navigation uses user language, consistent placement, and clear hierarchy
    - Breadcrumbs, search, and clear current-location cues support orientation
  </example>
  <use_case>Restructuring navigation for a growing content site or app</use_case>
</pattern>

<pattern name="modal_open_close_contract">
  <description>The behavioral contract a modal or overlay must satisfy once the decision to use one has been made</description>
  <decision_tree name="when_to_use">
    <question>Have you established that a blocking overlay is the right pattern (see the modal_overuse anti-pattern first)?</question>
    <if_yes>Implement the full open/close contract below; a partial implementation strands keyboard and assistive-technology users</if_yes>
    <if_no>Prefer inline UI, a drawer, or a non-blocking notification</if_no>
  </decision_tree>
  <example>
    On open: capture the element that had focus, then move focus to the first enabled focusable control inside the overlay.
    While open: trap Tab and Shift+Tab over a live query of focusable descendants, not a snapshot taken at open time — anything rendered later (a lazily loaded list, an expanded section) must be reachable.
    Escape: routes through the identical close path as the close button, so both run the same teardown.
    On close: return focus to the captured element.
    Throughout: keep `aria-hidden`, `aria-disabled`, and the native `disabled` attribute synchronized with the visual state, and expose `aria-haspopup="dialog"` plus `aria-controls` on the trigger.
  </example>
  <note>The snapshot-versus-live distinction is the one most often missed: a focus trap built from the descendants present at open time silently excludes every control the overlay renders afterwards.</note>
  <use_case>Auditing a dialog, settings overlay, or command palette for keyboard and screen-reader completeness</use_case>
</pattern>

<pattern name="refusing_to_close">
  <description>When a modal holds transient state that cannot be settled, refusing to close is correct — and every side effect that normally accompanies closing must be suppressed as a unit</description>
  <decision_tree name="when_to_use">
    <question>Can the overlay hold user state (a dragged item, an in-flight edit, an unassigned selection) that has nowhere to go if the overlay disappears?</question>
    <if_yes>Make close conditional: on refusal, keep the overlay open, keep its key listener active, and suppress every accompanying close effect</if_yes>
    <if_no>Close unconditionally and let the standard teardown run</if_no>
  </decision_tree>
  <example>
    Closing typically bundles several effects: hide the overlay, tear down the key listener, play a sound, resume the underlying view, restore focus.
    When close is refused, all of them must be suppressed together — decided once at the close path, not independently by each caller.
    Explain the refusal in place ("Your inventory is full — free a slot to put this item away") rather than silently ignoring the Escape key.
  </example>
  <note>Leaving the key listener torn down while the overlay stays visible produces an overlay the keyboard cannot reach, which is worse than either outcome alone.</note>
  <use_case>Editors, item pickers, and drag-and-drop surfaces where dismissal would destroy user work</use_case>
</pattern>

<pattern name="severity_switched_overlay">
  <description>One overlay element serving both "work in progress" and "this failed" needs opposite assistive-technology semantics for each</description>
  <decision_tree name="when_to_use">
    <question>Does a single surface display both a loading state and a failure state?</question>
    <if_yes>Reuse the element, but switch role, live-region politeness, modality, and focus behavior by severity</if_yes>
    <if_no>Use the semantics appropriate to the single state the surface represents</if_no>
  </decision_tree>
  <example>
    Loading: `role="status"`, `aria-live="polite"`, `aria-busy="true"`, no dialog semantics, focus left where the user put it.
    Fatal error: `role="alertdialog"`, `aria-live="assertive"`, `aria-modal="true"`, focus moved to the overlay so the message is unavoidable.
    The assertive announcement must carry the recovery path in the same message ("Couldn't load your session. Sign in again to continue."), not just the failure.
  </example>
  <note>Reusing the element is right; reusing its semantics is not. Announcing progress assertively interrupts for no reason, and announcing a fatal error politely lets it pass unnoticed.</note>
  <use_case>Startup and session overlays, data-loading panels that also render load failures</use_case>
</pattern>

<pattern name="settings_commit_classification">
  <description>Classify each setting as immediate-apply or review-before-commit rather than choosing one commit model for the whole panel</description>
  <decision_tree name="when_to_use">
    <question>Has someone proposed removing the Apply/Save button because it is friction?</question>
    <if_yes>Classify each setting by reversibility and cost, then replace what the draft/cancel model provided</if_yes>
    <if_no>Still audit the panel for settings whose current model does not match their cost</if_no>
  </decision_tree>
  <example>
    Immediate-apply suits low-cost, reversible toggles whose effect is visible at once.
    Review-before-commit suits expensive or disruptive options — anything that restarts a session, discards work, or takes visible time to undo.
    Removing the Apply button removes three things the user relied on: the draft/cancel mental model, a visible signal of what is saved, and a rollback point. Replace them with persistence feedback ("Saved") and an undo affordance.
    Define what a setting displays when the platform cannot honor it, so an option that silently does nothing is not indistinguishable from one that failed.
    Coalesce or throttle continuous inputs; a slider drag must not become fifty writes.
  </example>
  <note>Immediate apply is not free convenience — it moves cost from the click to the recovery. Settings that are cheap to change and cheap to change back are the ones where that trade is worth making.</note>
  <use_case>Redesigning a preferences surface, or reviewing a proposal to make settings apply instantly</use_case>
</pattern>

<pattern name="keyboard_shortcut_ownership">
  <description>Rules that keep document-level keyboard shortcuts from colliding with each other and with text entry</description>
  <decision_tree name="when_to_use">
    <question>Does the product bind shortcuts on a document-level listener, or does the same key mean different things in different modes?</question>
    <if_yes>Apply mode-gated ownership, an editable-target guard on keydown, and unconditional keyup processing</if_yes>
    <if_no>Standard per-component key handling is sufficient</if_no>
  </decision_tree>
  <example>
    Exactly one owner at a time: a key claimed by two features is consumed only by the currently visible one, gated on that surface being open. When it closes, the background feature gets the key back — the check belongs at the point of consumption, not in a registration table.
    Editable-target guard: on keydown, return before mutating any shortcut state when the target is an `input`, `textarea`, `select`, or `contentEditable` element. Skipping only the action is not enough — recording the key as held is itself the leak.
    Keyup is unconditional: process every release regardless of target. Focus can move into a field while a key is down, and if the release is filtered out the key stays held forever.
    A shortcut that opens a text surface (a chat or search box) must consume its own event so the newly focused field does not also receive the character.
  </example>
  <note>These three rules cover the three recurring bugs: the wrong feature responds, typing leaks into the background, and a key sticks after focus moves.</note>
  <use_case>Adding a global shortcut to an app that already has modal surfaces and text inputs</use_case>
</pattern>

<pattern name="held_input_lifecycle">
  <description>A held interaction needs an explicit release on every discontinuity, and a completed action must not re-fire from the same unreleased press</description>
  <decision_tree name="when_to_use">
    <question>Does any interaction depend on an input staying held — press-and-hold, drag, long-press, key repeat?</question>
    <if_yes>Define the discontinuity set and a completion latch, and clear both through the same reset path</if_yes>
    <if_no>Ordinary click and tap handling is sufficient</if_no>
  </decision_tree>
  <example>
    Release on discontinuity: clear held state when an overlay opens, the view pauses, the window blurs, the document becomes hidden, or navigation occurs. Without this the user returns to a UI that believes a button is still pressed.
    Consume the whole gesture on completion: once a held action completes, latch it until the actual release. Resetting only the progress indicator is not enough — the target under the pointer has usually changed as a result of the action, and a still-held input immediately starts the next one.
    Clear the latch through the same discontinuity and reset paths as the held state, so the two cannot drift apart.
  </example>
  <note>The two failure modes are complementary: missing discontinuity handling gives stuck input, and a missing completion latch gives unintended repeats against freshly changed state.</note>
  <use_case>Press-and-hold delete, drag-to-reorder, long-press menus, hold-to-confirm actions</use_case>
</pattern>
</patterns>

<best_practices>
  <practice priority="critical">
    <name>Meet minimum text contrast</name>
    <description>Ensure text and essential UI meet WCAG contrast ratios so content is perceivable</description>
    <example>
      Normal text: at least 4.5:1 against its background.
      Large text and UI components / graphical objects: at least 3:1.
      Never communicate meaning through color alone; pair with text, icon, or pattern.
    </example>
  </practice>

  <practice priority="critical">
    <name>Guarantee keyboard operability and visible focus</name>
    <description>Every interactive element must be reachable and operable by keyboard with a clearly visible focus indicator</description>
    <example>
      All actions reachable via Tab/Shift+Tab in a logical order.
      Focus indicator is always visible and high-contrast.
      No keyboard traps; modals trap focus only intentionally and return it on close.
    </example>
  </practice>

  <practice priority="critical">
    <name>Prevent errors before they happen</name>
    <description>Design out error-prone conditions and confirm consequential or irreversible actions</description>
    <example>
      Constrain input to valid values; use correct input types and selection over free text.
      Confirm or require explicit intent before destructive actions; offer undo where possible.
    </example>
  </practice>

  <practice priority="critical">
    <name>Provide timely system feedback</name>
    <description>Acknowledge every user action and communicate ongoing system state</description>
    <example>
      Respond to interactions within the Doherty threshold (~400ms) or show progress.
      Use loading, success, and error states; never leave an action without acknowledgment.
    </example>
  </practice>

  <practice priority="high">
    <name>Write actionable, blame-free error messages</name>
    <description>Errors should state the problem in plain language and offer a constructive path to recovery</description>
    <example>
      Good: "Your password needs at least 8 characters. Add a few more."
      Avoid: "Error 0x5: invalid input." or "You entered the password wrong."
      Place the message near the affected field and preserve the user's input.
    </example>
  </practice>

  <practice priority="high">
    <name>Reduce choices and cognitive load</name>
    <description>Apply Hick's Law and Miller's Law to limit and chunk decisions</description>
    <example>
      Group related options; reveal advanced choices progressively.
      Highlight a recommended default to reduce decision time.
      Chunk long sequences (numbers, steps) into meaningful groups.
    </example>
  </practice>

  <practice priority="high">
    <name>Follow established conventions</name>
    <description>Apply Jakob's Law by matching patterns users already know from other products</description>
    <example>
      Place navigation, search, and cart icons where users expect them.
      Use conventional icons with text labels; deviate only with clear benefit.
    </example>
  </practice>

  <practice priority="high">
    <name>Design clear visual hierarchy</name>
    <description>Use size, weight, contrast, spacing, and position to signal importance and guide the eye</description>
    <example>
      One primary call-to-action per view, visually distinct (Von Restorff Effect).
      Establish a single clear focal point; subordinate secondary content.
    </example>
  </practice>

  <practice priority="high">
    <name>Respect motion and reduced-motion preferences</name>
    <description>Provide a reduced-motion alternative for animation and avoid motion that can trigger discomfort</description>
    <example>
      Honor the operating-system reduced-motion preference.
      Avoid large parallax, autoplay motion, and flashing beyond safe thresholds.
    </example>
  </practice>

  <practice priority="high">
    <name>Make labels persistent and programmatic</name>
    <description>Every input needs a visible, persistent label that is also exposed to assistive technology</description>
    <example>
      Use top-aligned labels that remain visible while typing.
      Do not rely on placeholder text as the only label.
    </example>
  </practice>

  <practice priority="medium">
    <name>Make empty states instructive</name>
    <description>Treat empty and first-run states as opportunities to guide users toward value</description>
    <example>
      Explain what the space is for, why it is empty, and the next action to take.
      Provide a clear primary action rather than a blank screen.
    </example>
  </practice>

  <practice priority="medium">
    <name>Optimize typography for readability</name>
    <description>Set line length, line height, and type scale for comfortable reading</description>
    <example>
      Keep body line length around 45-75 characters.
      Use comfortable line height and a consistent modular type scale.
    </example>
  </practice>

  <practice priority="medium">
    <name>Design memorable peaks and endings</name>
    <description>Apply the Peak-End Rule to shape how the experience is remembered</description>
    <example>
      Reduce friction at the most painful moment of a flow.
      End tasks with a clear, satisfying confirmation or moment of delight.
    </example>
  </practice>

  <practice priority="medium">
    <name>Use a consistent spacing system</name>
    <description>Adopt a spacing scale (commonly an 8pt grid) and apply Gestalt grouping</description>
    <example>
      Use proximity and common region to group related elements.
      Apply consistent spacing increments for rhythm and alignment.
    </example>
  </practice>
</best_practices>

<anti_patterns>
  <avoid name="low_contrast_text">
    <description>Text or essential UI with insufficient contrast against its background</description>
    <instead>Meet at least 4.5:1 for normal text and 3:1 for large text and UI components; verify with a contrast checker</instead>
  </avoid>

  <avoid name="removed_focus_outline">
    <description>Removing or hiding the focus indicator, stranding keyboard and assistive-technology users</description>
    <instead>Always provide a clearly visible, high-contrast focus indicator; restyle it rather than removing it</instead>
  </avoid>

  <avoid name="layout_shift">
    <description>Content that jumps as images, ads, fonts, or late content load, causing CLS and misclicks</description>
    <instead>Reserve explicit space for media and dynamic content; keep CLS at or below 0.1</instead>
  </avoid>

  <avoid name="placeholder_as_label">
    <description>Using placeholder text as the only field label, which disappears on input and fails accessibility</description>
    <instead>Use a persistent, programmatically associated label; reserve placeholders for example formatting only</instead>
  </avoid>

  <avoid name="dark_patterns">
    <description>Manipulative tactics such as confirmshaming, forced continuity, and roach motel that exploit users</description>
    <instead>Be transparent about costs and consequences; make opting out and cancelling as easy as opting in</instead>
  </avoid>

  <avoid name="modal_overuse">
    <description>Interrupting users with frequent or stacked modal dialogs that break flow and trap focus</description>
    <instead>Reserve modals for focused, blocking decisions; prefer inline UI, drawers, or non-blocking notifications otherwise</instead>
  </avoid>

  <avoid name="nested_modal_focus_contention">
    <description>Two stacked overlays both running a focus trap, so the lower one's bubbling Tab handler fights the upper one's traversal and the top dialog becomes unnavigable by keyboard</description>
    <instead>Make focus ownership exclusive: while a higher surface is open, the lower surface's key handler and focus trap return immediately. The bug is invisible to mouse users and total for keyboard users, so it survives manual testing</instead>
  </avoid>

  <avoid name="focus_dropped_on_rerender">
    <description>Re-rendering a list or panel replaces the DOM node that had focus, so focus falls to the document body and keyboard navigation restarts from the top</description>
    <instead>After a re-render that replaces the active element, move focus to the corresponding new node. A re-render is a focus handover, not a neutral repaint</instead>
  </avoid>

  <avoid name="snapshot_focus_trap">
    <description>Building a focus trap from the focusable elements present when the overlay opened, stranding anything rendered afterwards outside the tab cycle</description>
    <instead>Query focusable descendants live on each traversal, filtered to visible and enabled controls</instead>
  </avoid>

  <avoid name="exact_target_identity_dispatch">
    <description>Routing pointer or click events by comparing `event.target` against a registered element roster, which misses taps that land on an icon, span, or SVG nested inside the registered control</description>
    <instead>Resolve the handler with `closest()` from the event target, or set `pointer-events: none` on decorative children. This breaks partially — the button works when tapped on its padding and not on its label — which makes it read as flakiness rather than a routing bug</instead>
  </avoid>

  <avoid name="unreleased_held_input">
    <description>Clearing held-input state only on the matching release event, so an overlay opening, a window blur, or a tab switch leaves the input stuck down indefinitely</description>
    <instead>Clear held state on every discontinuity, and process keyup and pointerup unconditionally even when focus has moved into an editable field</instead>
  </avoid>

  <avoid name="infinite_scroll_for_findability">
    <description>Using infinite scroll where users need to find, return to, or compare specific items</description>
    <instead>Use pagination or load-more with stable URLs and reachable footers for findability-critical content</instead>
  </avoid>

  <avoid name="disabled_button_without_reason">
    <description>Disabling a button without explaining what is required to enable it</description>
    <instead>Explain what is missing, or keep the control active and surface a clear validation message on attempt</instead>
  </avoid>

  <avoid name="carousel_for_key_content">
    <description>Hiding important content in auto-rotating carousels that users rarely see beyond the first slide</description>
    <instead>Present key content directly and persistently; avoid burying primary messages in rotating panels</instead>
  </avoid>

  <avoid name="motion_without_fallback">
    <description>Animation and motion with no reduced-motion alternative, risking discomfort and accessibility failures</description>
    <instead>Honor reduced-motion preferences and provide a static or minimal-motion alternative</instead>
  </avoid>
</anti_patterns>

<rules priority="standard">
  <rule>Cite only established frameworks, laws, and standards; never fabricate heuristic names or numeric thresholds</rule>
  <rule>Prefer behavioral evidence (usability testing) over opinion when claims are contested</rule>
  <rule>Treat WCAG conformance as a baseline, not the ceiling, for inclusive design</rule>
  <rule>Improve perceived performance alongside measured performance, never as a substitute for fixing real latency</rule>
  <rule>Respect existing product and platform conventions unless deviation has a clear, tested benefit</rule>
  <rule>Give exactly one open surface ownership of keyboard containment; stacked traps must defer to the topmost, not run in parallel</rule>
  <rule>Route every dismissal path (close button, Escape, backdrop) through one close implementation so teardown cannot diverge between them</rule>
  <rule>Choose a commit model per setting from its reversibility and cost; never remove an explicit save without replacing the persistence feedback and rollback it provided</rule>
  <rule>Treat a held interaction as unfinished until an explicit release or a discontinuity clears it, and verify the discontinuity set rather than fixing stuck-input reports one at a time</rule>
</rules>

<error_escalation>
  <examples>
    <example severity="low">Minor inconsistency in spacing or label casing</example>
    <example severity="medium">Form lacks inline validation or recovery guidance</example>
    <example severity="high">Key flow is not keyboard operable or fails contrast minimums</example>
    <example severity="critical">Interface uses dark patterns or blocks assistive-technology users from completing a core task</example>
  </examples>
</error_escalation>

<related_agents>
  <agent name="design">Verify design-system consistency and architecture-level UX decisions</agent>
  <agent name="quality-assurance">Review interface changes for usability and accessibility regressions</agent>
  <agent name="performance">Investigate and improve measured Core Web Vitals</agent>
</related_agents>

<constraints>
  <must>Ground every recommendation in an established heuristic, law, or standard</must>
  <must>Treat accessibility (WCAG POUR) as a first-class requirement, not an add-on</must>
  <must>Distinguish measured performance from perceived performance</must>
  <avoid>Inventing heuristic names or numeric thresholds</avoid>
  <avoid>Recommending dark patterns or manipulative design</avoid>
  <avoid>Letting aesthetics substitute for tested usability</avoid>
</constraints>

<related_skills>
  <skill name="game-ux">Companion design skill for player-facing experience, game feel, and game accessibility</skill>
  <skill name="requirements-definition">Define UX acceptance criteria and usability requirements before implementation</skill>
  <skill name="technical-documentation">Produce user guides, help content, and onboarding documentation</skill>
  <skill name="technical-writing">Craft clear microcopy, help content, and UX writing</skill>
</related_skills>
