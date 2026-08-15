---
name: game-ux
description: Use for game player-experience design - game feel/juice, onboarding, difficulty pacing, HUD/UI readability, and accessibility, grounded in MDA, flow, and player-motivation frameworks.
version: 3.0.0
---

Engine-agnostic patterns for game player-experience design: game feel, onboarding, difficulty pacing, HUD
readability, and accessibility. Grounded in MDA, Flow, and player-motivation theory only where the framework
changes what you'd build.

## Diagnosing "it isn't fun" — MDA

Mechanics (rules/data) produce Dynamics (runtime behavior) which produce Aesthetics (the player's emotional
response) — designers build left-to-right, players experience right-to-left. **When a feature feels wrong,
trace back from the target aesthetic to the dynamic producing it, then to the mechanic you can actually edit**;
editing the aesthetic directly (more VFX, more text) treats the symptom.

"Fun" is not one thing — MDA names eight distinct pleasures: Sensation, Fantasy, Narrative, Challenge,
Fellowship, Discovery, Expression, Submission. Name the two or three a feature actually targets and playtest
against those, not a vague notion of fun.

## Game feel and juice

Game feel is the felt (not read) quality of real-time control, built from input latency, simulated-space
interaction, and polish. **Input responsiveness is the single largest lever on game feel — acknowledge input
on the same frame it arrives, even when the full action resolves later**; long non-interruptible animations,
wrong-order input buffering, and high latency all break the control illusion. A responsive character with
placeholder art feels good; a floaty character with great art does not.

Juice (screen shake, particles, hit-stop, squash-and-stretch, sound) is layered, non-essential feedback that
amplifies an action that is already mechanically sound. **Juice cannot rescue a broken core loop or laggy
input — fix responsiveness and the mechanic first.** Every input should produce a visible, audible, or
kinesthetic response. Guardrail: if juice at peak intensity occludes critical information (health, threats),
dial it back — readability outranks spectacle.

## Core loop

The core loop (action → feedback → reward → progression) is the atomic, minute-to-minute unit of the
experience. **Grey-box test the bare verb with no UI, no rewards, no story — if repeating it isn't fun on its
own, no reward layer will fix it**; only after that, define what's gained (reward) and how the next iteration
differs (progression).

## Difficulty and pacing — Flow

Csikszentmihalyi's flow channel runs between anxiety (challenge > skill) and boredom (skill > challenge); as
skill grows over a session, challenge must rise to match, with clear goals and immediate feedback so the
player can self-correct.

**Distinguish fair-hard (readable, learnable) from unfair (random, unreadable) — tune the former's magnitude,
eliminate the latter entirely.** Concrete criteria:
- Every serious threat gets a clear, consistent telegraph (wind-up animation + audio cue) the player can learn
  to read and dodge. An instant, unreadable hit is fake difficulty, not challenge.
- Introduce one new challenge type at a time; let it be practiced alone before combining with others.
- Prefer sawtooth pacing (spike, then a recovery valley) over a monotone climb.
- Offer difficulty/assist options as accessibility, not "easy mode" shame — they let a player choose their own
  position in the flow channel without you having to guess it.
- Dynamic Difficulty Adjustment only where the adaptation is invisible enough not to read as the game cheating
  in either direction.

Feedback loops at the system level either amplify a lead (positive: snowballing, shortens games, risks
blowouts) or dampen it (negative: rubber-banding, keeps games close, feels unfair if too strong). Tune the mix
to the drama you want, not by instinct.

## Onboarding — FTUE

**Teach by doing, just in time, one concept at a time — the player performs the core verb within the first
moments, before reading anything**; each new mechanic is introduced the moment it's first needed, in a safe
space where it's the only available action, with increasing complexity as steps are mastered. No wall-of-text.
Tutorials must be skippable for experienced/repeat players. Validate with a silent playtest: do new players
reach the core loop unaided? If not, the first minutes are where retention is lost.

## HUD and readability

Fagerholt and Lorentzon's four-quadrant model classifies every UI element by fiction × spatial-presence:
Diegetic (in fiction and world — e.g. an in-world ammo counter), Non-diegetic (outside both — a classic HUD
overlay), Spatial (in the 3D space, not the fiction — floating waypoint markers), Meta (in the fiction, not the
space — blood on the screen edge). **Diegetic deepens immersion but costs readability; non-diegetic is
clearest but breaks fiction — choose per element by weighing immersion against the cost of misreading it**
(e.g., a low-health warning is worth breaking fiction for; an ammo count usually isn't).

Cognitive load is the amount of information the player must process at once; managing it is the readability
job. For every HUD element, ask: needed always, contextually, or never — hide the non-essential. Use a
consistent color/shape/position language so critical info (health, threats) reads pre-attentively: the same
color always means the same thing, everywhere. Verify readability at the target resolution and viewing
distance (couch vs. desktop), and reserve safe zones so juice/VFX at peak intensity never occludes the HUD.

Lead with the world (lighting, composition, leading lines, color) before resorting to quest markers and
waypoints — strong environmental affordances let players learn without an overt marker at all; reserve markers
for when environmental signposting alone fails.

## Accessibility

Codified by the Game Accessibility Guidelines and Xbox Accessibility Guidelines. **The core rule: never convey
anything critical through a single sensory channel** — pair color with icon/shape/label, pair audio cues with
a visual indicator, caption key sound effects. Concrete criteria by category:
- Motor: fully remappable controls; no required rapid mashing or precise timing without an alternative;
  toggle-vs-hold options.
- Visual: scalable UI/text; colorblind modes; never color-only information; high-contrast option; screen-reader
  support for menus.
- Hearing: subtitles/captions with speaker labels and key sound-effect captions; visual redundancy for
  directional/audio cues (e.g. a directional damage indicator).
- Cognitive: difficulty/assist options; persistent objective reminders; a reduced-time-pressure option.
- Motion: reduced-motion / disable-camera-shake options; motion-sickness mitigations (FOV control, vignette).
- Discoverability: surface accessibility options early, and make them changeable at any time, not just at
  first launch.

Design this in from day one — it is cheapest designed in, costliest bolted on after ship.

## Motivation and rewards

Self-Determination Theory: intrinsic motivation (play for its own sake, which sustains) rests on autonomy
(meaningful choice), competence (legible mastery and growth), relatedness (genuine social connection).
Extrinsic reward can crowd out intrinsic interest — **watch for reward systems that replace genuine interest
with grind engineered to be relieved by spending.**

Bartle's player types (Achiever/Explorer/Socializer/Killer) and Quantic Foundry's six motivation clusters
(Action, Social, Mastery, Achievement, Immersion, Creativity) are vocabularies for checking a design serves
more than one motivation — useful as a cross-check on multiplayer/social systems, not a personality test to
design around rigidly.

Variable-ratio reward schedules (reward after an unpredictable number of actions) produce the most persistent
engagement — the same mechanism as slot machines. **This power carries an ethical duty: use it to sustain
healthy engagement, never to exploit compulsion or pair it with a spending prompt.** Prefer rewards that
express skill over pure chance.

## Ethical design

Respect player time, money, and agency. Avoid: predatory loot boxes, pay-to-win, manipulative FOMO and
fear-based retention, deceptive UI, grind engineered to sell relief. **If a tactic relies on the player not
understanding it, that's the test that it's manipulation, not persuasion** — don't ship it.

## Playtesting

Structured observation is how any of the above claims get validated, not assumed. **Observe, do not lead** —
recruit target-audience testers who have not seen the game, define what you want to learn beforehand
(onboarding? a specific difficulty spike?), then stay silent or use think-aloud and record every hesitation,
wrong turn, misread, or quit as data. Do not explain the game or defend a decision mid-session — if you have to
explain it in the room, you'll have to explain it in the wild. Debrief after, with open (non-leading)
questions, then iterate on the single biggest friction point and re-test with fresh players.

## Related

- [technical-writing](../technical-writing/SKILL.md) — write clear in-game text, tutorials, and tooltips
- [requirements-definition](../requirements-definition/SKILL.md) — define experience pillars and UX acceptance
  criteria
- [investigation-patterns](../investigation-patterns/SKILL.md) — root-cause a UX problem via the MDA
  mechanics-dynamics-aesthetics chain
- [testing-patterns](../testing-patterns/SKILL.md) — structure playtest sessions and validate UX hypotheses
