---
name: web-ux
description: Use for web/app UX work - usability review, accessibility (WCAG), form and onboarding design, information architecture, and interaction mechanics like modal focus ownership, dialog open/close contracts, and keyboard-shortcut ownership.
version: 3.0.0
---

Framework-agnostic UX heuristics, accessibility thresholds, and interaction-mechanics rules for evaluating and
building usable, accessible, trustworthy interfaces. The named laws below are a checklist for citing evidence,
not a tutorial; the interaction-mechanics sections that follow are the part most often gotten wrong.

## Heuristics and laws to cite

Nielsen-Norman's 10 usability heuristics (visibility of system status, match with the real world, user control
and freedom, consistency and standards, error prevention, recognition over recall, flexibility and efficiency,
aesthetic and minimalist design, help users recognize/diagnose/recover from errors, help and documentation) and
the standard perception/decision laws — Hick's (decision time grows with choice count), Fitts's (target
acquisition time is a function of distance and size), Miller's (~7±2 working-memory chunks), Jakob's (users
expect your product to work like the ones they already know), Tesler's (complexity is conserved, only moved
between system and user), Doherty Threshold (~400ms response keeps neither party waiting), Peak-End Rule
(experiences are judged by their peak and their ending), Serial Position Effect (first and last items are
best recalled), Aesthetic-Usability Effect (attractive design reads as more usable and buys tolerance for
minor flaws), Von Restorff Effect (the one item that differs from its neighbors is what gets remembered), and
Postel's Law (accept varied input, produce well-formed output) — are the vocabulary for justifying a finding.
Cite the specific law; never fabricate a heuristic name or a numeric threshold that isn't one of the numbers
below.

Rate collected usability issues on the 0-4 severity scale (0 = not a problem, 4 = catastrophe, imperative
before release), weighing frequency, impact, and persistence rather than a straight tally of issue count.

## Accessibility and performance thresholds

The numbers actually worth checking; skip restating the WCAG POUR framing around them.

- Contrast: 4.5:1 normal text, 3:1 large text and UI components/graphical objects. Never encode meaning in
  color alone.
- Target size: 24x24 CSS px minimum (WCAG 2.2 AA, SC 2.5.8), 44x44 enhanced (AAA, SC 2.5.5) — use the 44px
  floor for touch interfaces regardless of which WCAG level is targeted.
- Keyboard: every action reachable via Tab/Shift+Tab in a logical order, a visible high-contrast focus
  indicator that is restyled rather than removed, no keyboard traps except an intentional modal trap that
  returns focus on close.
- Every input has a persistent, programmatically-associated label — placeholder text alone fails both
  usability (it disappears on input) and accessibility (it isn't exposed as a label).
- Status messages and dynamic content changes are exposed to assistive tech (`role`, `aria-live`, name/role/
  value), not just indicated visually.
- Core Web Vitals: LCP <=2.5s, INP <=200ms, CLS <=0.1. When real latency can't hit these, close the gap with
  skeleton screens, optimistic UI, and reserved layout space — perceived-performance techniques mask latency,
  they don't substitute for fixing it.
- Body text line length ~45-75 characters; spacing on a consistent scale (commonly an 8pt grid).

## Silent focus and overlay failures

These bugs are invisible to a mouse user and total for a keyboard or screen-reader user, so they survive
manual testing done by sighted mouse users and ship anyway.

**Focus ownership is exclusive, not additive.** At any moment exactly one open surface owns Tab traversal and
Escape. When surfaces stack (a settings panel over a pause screen, a drawer over a modal), every surface
beneath the topmost must return from its key handler immediately. Two stacked overlays each running their own
trap causes the lower one's bubbling Tab handler to fight the upper one's traversal, and the top dialog becomes
unnavigable by keyboard — a bug mouse users never see.

**Build the focus trap from a live query, not a snapshot.** A trap built from the focusable descendants present
at open time silently excludes anything the overlay renders afterward — a lazily loaded list, an expanded
section. Query focusable descendants live on each traversal, filtered to visible and enabled controls.

**A re-render is a focus handover, not a neutral repaint.** Replacing the DOM node that had focus drops focus
to `document.body` and restarts keyboard navigation from the top. After a re-render that replaces the active
element, move focus to its replacement explicitly.

**The open/close contract**, once a blocking overlay is warranted: on open, capture the previously focused
element and move focus to the first enabled control inside the overlay; Escape must route through the same
close path as the close button so teardown can't diverge between them; on close, return focus to the captured
element; keep `aria-hidden`/`aria-disabled`/`disabled` synchronized with visual state, and put
`aria-haspopup="dialog"` plus `aria-controls` on the trigger.

**A refused close must suppress every accompanying effect as one unit.** When an overlay holds state that has
nowhere to go if it disappears (a dragged item, an in-flight edit), refusing to close is correct — but closing
normally bundles hiding the overlay, tearing down the key listener, playing a sound, restoring focus, and
resuming the underlying view. Suppress all of them together at the close path, not independently per caller;
leaving the key listener torn down while the overlay stays visible produces an overlay the keyboard can't
reach, which is worse than not refusing at all. Explain the refusal in place rather than silently swallowing
Escape.

**One element serving both a loading and a failure state needs opposite ARIA semantics for each**, switched by
severity rather than fixed at the element: loading gets `role="status"`, `aria-live="polite"`,
`aria-busy="true"`, no dialog semantics, focus left where the user put it; a fatal error gets
`role="alertdialog"`, `aria-live="assertive"`, `aria-modal="true"`, and focus moved onto the overlay. Put the
recovery path in the same assertive message as the failure ("Couldn't load your session. Sign in again."), not
just the failure.

**Route pointer/click handling through `closest()`, not `event.target` identity comparison.** Comparing
`event.target` against a registered element roster misses taps landing on an icon, span, or SVG nested inside
the control. This fails partially — it works when tapped on padding, not on the label — so it reads as
flakiness rather than a routing bug.

## Settings commit models

Classify each setting individually as immediate-apply or review-before-commit by its reversibility and cost,
not one model for the whole panel. Low-cost, reversible, visible-at-once toggles suit immediate apply.
Anything that restarts a session, discards work, or takes visible time to undo belongs behind
review-before-commit. Removing an Apply/Save button removes three things at once — the draft/cancel mental
model, a visible signal of what's saved, and a rollback point — so replace them with persistence feedback
("Saved") and an undo affordance rather than just deleting the button. Define what a setting shows when the
platform can't honor it, so a silently-no-op option isn't indistinguishable from a failed one. Coalesce or
throttle continuous inputs — a slider drag must not become fifty writes.

## Keyboard shortcuts and held input

**Shortcut ownership is exclusive and checked at the point of consumption, not in a registration table.** A key
claimed by two features is consumed only by whichever is currently visible; when that surface closes, the
background feature gets the key back. On keydown, return before mutating any shortcut state when the target is
`input`/`textarea`/`select`/`contentEditable` — skipping only the action isn't enough, since recording the key
as held is itself the leak. Process keyup unconditionally regardless of target, because focus can move into a
field while a key is down, and a filtered-out release leaves the key stuck held forever. A shortcut that opens
a text surface (chat, search) must consume its own keydown event so the newly focused field doesn't also
receive the character.

**A held interaction (press-and-hold, drag, long-press, key repeat) needs an explicit release on every
discontinuity** — an overlay opening, the view pausing, window blur, `visibilitychange`, navigation, pointer
capture loss, or focus moving into an editable field. Missing any one of these leaves the user looking at a UI
that still believes a button is pressed. Enumerate the discontinuity set as a checklist up front rather than
patching it case-by-case as bugs are reported.

**Latch a completed held action until the real release fires**, cleared through the same reset path as the
discontinuity handling so the two can't drift apart. Resetting only the progress indicator isn't enough: the
target under the pointer has usually changed as a result of the completed action, so a still-held input
immediately restarts against different state.

## Other checkable failure modes

- **Disabled button with no stated reason**: either explain what's missing to enable it, or keep the control
  active and surface a clear validation message on attempt.
- **Infinite scroll where users need to find, return to, or compare specific items**: use pagination or
  load-more with stable URLs and a reachable footer instead.
- **Dark patterns** (confirmshaming, forced continuity, roach motel) make cancelling harder than subscribing —
  treat opt-out and opt-in as symmetric in effort.

## Related

- [game-ux](../game-ux/SKILL.md) — companion skill for player-facing experience, game feel, and game
  accessibility.
- [requirements-definition](../requirements-definition/SKILL.md) — define UX acceptance criteria and usability
  requirements before implementation.
- [technical-documentation](../technical-documentation/SKILL.md) — produce user guides, help content, and
  onboarding documentation.
- [technical-writing](../technical-writing/SKILL.md) — craft clear microcopy, help content, and UX writing.
