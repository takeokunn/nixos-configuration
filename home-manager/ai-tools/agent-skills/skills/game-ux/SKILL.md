---
name: Game UX
description: This skill should be used when the user asks to "improve game UX", "game feel / juice", "player onboarding", "difficulty/pacing", "HUD/UI design", "game accessibility", or designs player experience. Provides world-class game UX best practices grounded in MDA, game feel, flow, and player-motivation frameworks.
version: 2.0.0
---

<purpose>
  Provide engine-agnostic, design-level game UX patterns and frameworks for crafting player experience: motivation, game feel, flow and pacing, onboarding, HUD/UI readability, accessibility, and ethical design. Grounded in established theory (MDA, Game Feel, Flow, Self-Determination Theory) rather than engine-specific implementation.
</purpose>

<concepts>
<concept name="mda_framework">
  <description>Mechanics, Dynamics, Aesthetics: a lens (Hunicke, LeBlanc, Zubek) for analyzing games as systems. Mechanics are the rules and data; Dynamics are the runtime behavior that emerges when players interact with mechanics; Aesthetics are the emotional responses evoked in the player.</description>
  <guidance>Designers build left-to-right (mechanics first), players experience right-to-left (aesthetics first). To change a feeling, trace back from the target aesthetic to the dynamic that produces it, then to the mechanic you can edit.</guidance>
  <scope>Whole-game analysis, feature framing, root-causing "the game isn't fun"</scope>
  <when>Diagnosing why a feature feels wrong, aligning a team on intended experience</when>
</concept>

<concept name="eight_kinds_of_fun">
  <description>The MDA aesthetics taxonomy of eight distinct player pleasures: Sensation (game as sense-pleasure), Fantasy (make-believe), Narrative (drama), Challenge (obstacle course), Fellowship (social framework), Discovery (uncharted territory), Expression (self-discovery / creation), Submission (pastime / pleasurable routine).</description>
  <guidance>"Fun" is not monolithic. Name the two or three aesthetics a game is actually targeting; design and playtest against those, not a vague notion of fun.</guidance>
  <when>Defining the experience pillars of a game or feature</when>
</concept>

<concept name="game_feel">
  <description>The tactile, kinesthetic sensation of controlling an avatar in real time (Steve Swink, "Game Feel"). Built from three layers: real-time control (the moment-to-moment input loop), simulated space (interaction with a believable game world), and polish (effects that amplify intent without changing rules).</description>
  <guidance>Game feel is felt, not read. It lives in the responsiveness of input and the quality of feedback, largely independent of graphical fidelity.</guidance>
  <scope>Character controllers, weapon handling, camera, anything the player directly drives</scope>
  <when>The mechanics are correct but control feels mushy, floaty, or dead</when>
</concept>

<concept name="juice">
  <description>Maximal output feedback for minimal player input ("Juice it or lose it", Jonasson and Purho). Layered, non-essential feedback — screen shake, particles, easing, squash-and-stretch, sound, hit-stop, color flashes — that makes interactions feel alive and rewarding.</description>
  <guidance>Juice amplifies an action that is already mechanically sound; it cannot rescue a broken core loop. Every input should produce a visible, audible, or kinesthetic response.</guidance>
  <when>Adding satisfaction to an already-working interaction</when>
</concept>

<concept name="input_responsiveness">
  <description>The perceived immediacy between player input and on-screen response, dominated by input latency (input lag) and animation commitment. The single most important contributor to good game feel.</description>
  <guidance>Minimize end-to-end latency; favor responsiveness over realism. Long non-interruptible animations, input buffering done wrong, and high latency all break the control illusion. Acknowledge input on the same frame even if the full action resolves later.</guidance>
  <scope>Every directly controlled action</scope>
  <when>Controls feel laggy, sluggish, or unresponsive</when>
</concept>

<concept name="flow">
  <description>Csikszentmihalyi's optimal-experience state of full, energized absorption. Sustained when perceived challenge and player skill are both high and roughly balanced — the "flow channel" running between anxiety (challenge exceeds skill) and boredom (skill exceeds challenge).</description>
  <guidance>As skill grows over a session, challenge must rise to match. Provide clear goals and immediate feedback so the player can self-correct and stay in the channel.</guidance>
  <scope>Session pacing, difficulty progression, mastery design</scope>
  <when>Players quit out of frustration or boredom</when>
</concept>

<concept name="difficulty_and_mastery">
  <description>The shape of challenge over time. A mastery curve gates new challenge behind demonstrated skill. Dynamic Difficulty Adjustment (DDA) adapts challenge to measured performance. Difficulty and assist options let players choose their own position in the flow channel.</description>
  <guidance>Treat difficulty options as accessibility, not as a compromise of vision. Separate "fair and hard" (readable, learnable) from "unfair" (random, unreadable). Telegraph threats clearly.</guidance>
  <when>Tuning challenge, widening audience without dumbing down</when>
</concept>

<concept name="core_loop">
  <description>The central repeated cycle of play — the action - reward - progression - new-action loop the player performs minute to minute (and the nested longer loops around it). The atomic unit of the experience.</description>
  <guidance>The core loop must be intrinsically satisfying in isolation, before any meta-progression. If the verb isn't fun on its own, no reward layer will fix it.</guidance>
  <scope>Whole-game structure, retention</scope>
  <when>Defining or repairing what the player actually does</when>
</concept>

<concept name="feedback_loops">
  <description>System-level loops that amplify (positive feedback: rich-get-richer, snowballing) or dampen (negative feedback: rubber-banding, catch-up) a player's lead. Distinct from UX feedback (the response to input).</description>
  <guidance>Positive loops shorten games and create decisive momentum but can cause runaway leads and blowouts; negative loops keep games close and tense but can feel unfair if too strong. Tune the mix to the desired drama.</guidance>
  <when>Balancing competitive or progression systems</when>
</concept>

<concept name="reward_schedules">
  <description>The timing and predictability of rewards. Fixed schedules are predictable; variable-ratio reinforcement (rewards after an unpredictable number of actions) produces the most persistent engagement — the same mechanism behind slot machines.</description>
  <guidance>Variable-ratio reward is powerful and therefore carries an ethical duty of care. Use it to sustain healthy engagement, never to exploit compulsion or pair it with spending. Prefer skill-expressing rewards over pure chance.</guidance>
  <when>Designing loot, progression, and reward cadence</when>
</concept>

<concept name="bartle_taxonomy">
  <description>Player-type model (Richard Bartle) along acting/interacting and players/world axes: Achievers (act on the world — points, completion), Explorers (interact with the world — discovery, systems), Socializers (interact with players — relationships), Killers (act on players — competition, domination).</description>
  <guidance>A useful vocabulary for MUD-derived multiplayer design, not a rigid personality test. Most players blend types and shift over time. Use it to check whether a design serves more than one motivation.</guidance>
  <when>Designing multiplayer and social systems</when>
</concept>

<concept name="self_determination_theory">
  <description>Psychological theory (Deci and Ryan) that intrinsic motivation rests on three needs: autonomy (meaningful choice and volition), competence (effective mastery and growth), and relatedness (connection to others). Applied to games via the PENS model.</description>
  <guidance>Intrinsic motivation (play for its own sake) sustains; extrinsic motivation (play for external reward) can crowd out intrinsic interest. Support all three needs; beware reward systems that replace genuine interest with grind.</guidance>
  <scope>Motivation design, retention without manipulation</scope>
  <when>Deciding why a player will keep playing</when>
</concept>

<concept name="gamer_motivation_model">
  <description>Quantic Foundry's empirically derived model clustering player motivations into six groups, each with two paired components: Action (Destruction + Excitement), Social (Competition + Community), Mastery (Challenge + Strategy), Achievement (Completion + Power), Immersion (Fantasy + Story), and Creativity (Design + Discovery).</description>
  <guidance>A data-grounded complement to Bartle for understanding audience. Identify which motivation clusters a game serves and confirm the design delivers on them.</guidance>
  <when>Audience definition, marketing-design alignment</when>
</concept>

<concept name="ftue">
  <description>First-Time User Experience: the player's onboarding, from first launch through learning the core loop. Tutorialization is the broader practice of teaching mechanics throughout the game.</description>
  <guidance>Teach by doing, just in time, one concept at a time, with increasing complexity. The first minutes decide retention. Let players act before they read.</guidance>
  <scope>Opening hour, mechanic introduction</scope>
  <when>Players churn early or don't understand the game</when>
</concept>

<concept name="diegetic_ui_classification">
  <description>Fagerholt and Lorentzon's four-quadrant model of game UI by two axes (fictional vs non-fictional, and spatial presence in the game's 3D world vs not): Diegetic (exists in the fiction and the world, e.g. an in-world ammo counter), Non-diegetic (outside fiction and space, e.g. a classic HUD overlay), Spatial (in the 3D space but not the fiction, e.g. floating waypoint markers), Meta (in the fiction but not the space, e.g. blood on the screen edges).</description>
  <guidance>Diegetic UI deepens immersion but can hurt readability; non-diegetic UI is clearest but breaks fiction. Choose per element by weighing immersion against the cost of misreading it.</guidance>
  <scope>HUD and UI element placement and style</scope>
  <when>Deciding how and where to surface information</when>
</concept>

<concept name="signposting_and_wayfinding">
  <description>Environmental and UI techniques that guide the player: signposting (clear cues toward goals), wayfinding (knowing where one is and where to go), breadcrumbing (a trail of small lures), and affordances (visual cues that communicate what an object does).</description>
  <guidance>Lead with the world (lighting, composition, leading lines, color) before resorting to overt markers. Strong affordances let players learn interactions without text.</guidance>
  <when>Players get lost or miss intended paths and interactions</when>
</concept>

<concept name="cognitive_load">
  <description>The amount of information and decision-making the player must process at once. High load reduces clarity and enjoyment; managing it is central to readable UX. Closely tied to the signal-to-noise ratio of the screen.</description>
  <guidance>Surface only what is needed when it is needed. Use color, shape, motion, and position as a consistent visual language so critical information reads pre-attentively. Balance juice against readability.</guidance>
  <scope>HUD density, combat readability, information architecture</scope>
  <when>The screen is cluttered or players miss critical information</when>
</concept>

<concept name="game_accessibility">
  <description>Designing so players with diverse abilities can play. Codified by the Game Accessibility Guidelines, the Xbox Accessibility Guidelines (XAGs), and advocacy from AbleGamers. Spans motor, visual, hearing, cognitive, and speech considerations.</description>
  <guidance>Accessibility widens the audience and improves UX for everyone. Provide redundancy: never convey critical information by a single channel (color alone, audio alone). Make options discoverable and changeable at any time.</guidance>
  <scope>Controls, captions, color, difficulty, motion, text scaling</scope>
  <when>From day one — accessibility is cheapest designed in, costliest bolted on</when>
</concept>

<concept name="lenses">
  <description>Jesse Schell's "The Art of Game Design: A Book of Lenses": a large set of focused questions ("lenses") for interrogating a design from many angles (the Lens of the Player's Mind, the Lens of Flow, the Lens of the Toy, the Lens of Accessibility, and many more).</description>
  <guidance>Use lenses as a structured questioning toolkit during review. Pick the few lenses relevant to the current problem rather than running all of them.</guidance>
  <when>Design review, breaking a design deadlock from a fresh angle</when>
</concept>

<concept name="ethical_design">
  <description>Respecting player time, money, and agency. Avoiding dark patterns: predatory loot boxes, pay-to-win, manipulative FOMO and fear-based retention, deceptive UI, and grind engineered to sell relief.</description>
  <guidance>Design for the player's genuine benefit and long-term trust over short-term extraction. Persuasion that the player would resent if they understood it is manipulation.</guidance>
  <scope>Monetization, retention, notifications</scope>
  <when>Any system that touches spending, time pressure, or compulsion</when>
</concept>

<concept name="playtesting">
  <description>Structured observation of real players to find where the experience breaks. Core methods include think-aloud protocol, observed (silent) sessions, and iteration. The first principle: observe, do not lead.</description>
  <guidance>Watch where players struggle without explaining the game to them — if you have to explain it in the room, you will have to explain it in the wild. Test early, test often, test with target-audience players who have not seen the game.</guidance>
  <scope>Validating onboarding, difficulty, readability, fun</scope>
  <when>Before assuming any UX decision is correct</when>
</concept>
</concepts>

<patterns>
<pattern name="core_loop_design">
  <description>Define and validate the central repeated action-reward-progression cycle so it is intrinsically satisfying before any meta layer.</description>
  <decision_tree name="when_to_use">
    <question>Is the moment-to-moment verb of the game unclear, or fun only because of rewards layered on top?</question>
    <if_yes>Strip to the core loop and make the bare verb satisfying first</if_yes>
    <if_no>Move outward to pacing, progression, or meta-loop tuning</if_no>
  </decision_tree>
  <example>
    Core-loop checklist:
    - Name the primary verb (the thing the player does most): _____
    - Is that verb fun with all rewards/UI stripped away? (test it grey-boxed)
    - Action -> immediate feedback: every action produces visible/audible response
    - Reward: what the player gains, and is it meaningful?
    - Progression: how the next iteration differs (new tools, harder challenge)
    - Loop length: seconds (micro) / minutes (core) / session (meta) all defined
    - Which of the 8 kinds of fun does the loop target?
  </example>
  <use_case>New game concept, retention problem traced to a hollow core verb</use_case>
</pattern>

<pattern name="difficulty_curve_tuning">
  <description>Shape challenge over time to keep players in the flow channel between anxiety and boredom.</description>
  <decision_tree name="when_to_use">
    <question>Are players quitting from frustration, or disengaging from boredom?</question>
    <if_yes>Map skill-vs-challenge over the session and re-shape the curve; consider difficulty/assist options</if_yes>
    <if_no>Verify challenge is fair and readable rather than adjusting its magnitude</if_no>
  </decision_tree>
  <example>
    Difficulty checklist:
    - Plot intended skill growth vs challenge across the first session
    - Introduce one new challenge type at a time; let it be practiced before combining
    - Sawtooth pacing: spikes followed by recovery valleys, not a monotone climb
    - Distinguish fair-hard (readable, learnable) from unfair (random, unreadable)
    - Telegraph every serious threat with a clear, consistent tell
    - Offer difficulty/assist options as accessibility, not as "easy mode shame"
    - Consider DDA only where invisible adaptation won't feel like cheating
  </example>
  <note>Fair difficulty is learnable; unfair difficulty is unreadable or random. Tune the former, eliminate the latter.</note>
</pattern>

<pattern name="ftue_design">
  <description>Design the first-time user experience to teach the core loop by doing, just in time, with progressive complexity.</description>
  <decision_tree name="when_to_use">
    <question>Do new players churn early or fail to understand the core loop?</question>
    <if_yes>Rebuild onboarding around teach-by-doing and just-in-time introduction</if_yes>
    <if_no>Audit mid-game tutorialization of later mechanics instead</if_no>
  </decision_tree>
  <example>
    FTUE checklist:
    - Player performs the core verb within the first moments (act before read)
    - Teach one mechanic at a time, the moment it is first needed (just in time)
    - Progressive complexity: each step builds on a mastered prior step
    - No wall-of-text; prefer demonstration, affordances, and safe practice space
    - Tutorials are skippable / non-blocking for experienced players
    - Early "safe failure" lets players learn consequences without punishment
    - Validate by silent playtest: do new players reach the core loop unaided?
  </example>
  <use_case>Onboarding redesign, high early-churn metrics</use_case>
</pattern>

<pattern name="juice_pass">
  <description>A polish pass that adds layered feedback to an already-working interaction to maximize satisfaction per input.</description>
  <decision_tree name="when_to_use">
    <question>Is the interaction mechanically correct but emotionally flat?</question>
    <if_yes>Run a juice pass to amplify the existing action with feedback</if_yes>
    <if_no>Fix the mechanic or input responsiveness first; juice cannot rescue broken feel</if_no>
  </decision_tree>
  <example>
    Juice checklist (apply tastefully, watch readability):
    - Input acknowledged on the same frame (responsiveness before all else)
    - Animation easing: anticipation and follow-through, squash and stretch
    - Hit feedback: hit-stop / freeze frames, knockback, flash on impact
    - Camera: subtle shake and kick scaled to impact (offer a reduce/disable option)
    - Particles and trails on key actions
    - Sound: layered, varied (avoid repetition fatigue), pitched to context
    - Tweened UI: numbers count up, elements spring rather than snap
    - Guardrail: does added juice ever obscure critical information? If so, dial back
  </example>
  <note>Juice multiplies a good action; it never substitutes for responsiveness or a sound core loop.</note>
</pattern>

<pattern name="hud_readability_review">
  <description>Audit the HUD and on-screen UI for signal-to-noise, clarity, and appropriate diegetic placement.</description>
  <decision_tree name="when_to_use">
    <question>Do players miss critical information or report a cluttered screen?</question>
    <if_yes>Run a readability review and cut or relocate low-value elements</if_yes>
    <if_no>Validate that the visual language is consistent across contexts</if_no>
  </decision_tree>
  <example>
    HUD readability checklist:
    - List every element; for each, classify (diegetic/non-diegetic/spatial/meta)
    - For each, ask: needed always, contextually, or never? Hide the non-essential
    - Critical info (health, threats) reads pre-attentively via position/color/shape
    - Consistent visual language: same color/shape means the same thing everywhere
    - Signal vs noise: remove decoration competing with actionable information
    - Readable at target resolution and viewing distance (couch vs desktop)
    - Does juice/VFX ever occlude the HUD at peak intensity? Reserve safe zones
  </example>
  <use_case>Cluttered combat UI, players missing low-health or objective cues</use_case>
</pattern>

<pattern name="accessibility_audit">
  <description>Systematically check a game against established accessibility guidelines, ensuring redundant channels for all critical information.</description>
  <decision_tree name="when_to_use">
    <question>Has the game been checked against accessibility guidelines with real redundancy?</question>
    <if_yes>Verify coverage and discoverability of each option</if_yes>
    <if_no>Run a full audit across motor, visual, hearing, and cognitive needs</if_no>
  </decision_tree>
  <example>
    Accessibility checklist (per Game Accessibility Guidelines / XAGs):
    - Motor: fully remappable controls; no required rapid mashing or precise timing without alternative; toggle vs hold options
    - Visual: scalable UI/text; colourblind modes; never colour-only information (add icon/shape/label); high-contrast option; screen-reader support for menus
    - Hearing: subtitles and captions to standard (speaker labels, key sound effects captioned); visual redundancy for audio cues (directional damage indicators)
    - Cognitive: difficulty/assist options; clear objectives and reminders; reduced time pressure option
    - Motion: reduced-motion / reduced-screen-shake / disable-camera-shake options; motion-sickness mitigations (FOV, vignette)
    - Discoverability: accessibility options surfaced early and changeable any time
  </example>
  <note>Redundancy is the core rule: never convey anything critical through a single sensory channel.</note>
</pattern>

<pattern name="playtest_session">
  <description>Run an observation-first playtest that reveals where the experience breaks without the designer leading the player.</description>
  <decision_tree name="when_to_use">
    <question>Are you about to ship a UX decision based on the team's own intuition?</question>
    <if_yes>Validate it with fresh target-audience players before committing</if_yes>
    <if_no>Use analytics/telemetry to confirm at scale once direction is validated</if_no>
  </decision_tree>
  <example>
    Playtest checklist:
    - Recruit target-audience testers who have NOT seen the game
    - Define what you want to learn (onboarding? difficulty? a specific moment?)
    - Do NOT explain the game; observe silently or use think-aloud protocol
    - Note where players hesitate, get lost, misread, or quit — these are the bugs
    - Resist the urge to defend or hint; record the friction faithfully
    - Debrief after, not during; ask open questions, avoid leading ones
    - Iterate on the biggest friction; re-test with new players
  </example>
  <use_case>Validating onboarding, difficulty, readability, and fun before launch</use_case>
</pattern>
</patterns>

<best_practices>
  <practice priority="critical">
    <name>Make input responsive above all</name>
    <description>Input responsiveness and low latency are the foundation of game feel; acknowledge input immediately, favor responsiveness over realism.</description>
    <example>
      On-frame input acknowledgement, interruptible animations, input buffering for queued actions, minimized end-to-end latency. A floaty character with great art still feels bad; a responsive character with primitive art feels good.
    </example>
  </practice>

  <practice priority="critical">
    <name>Make the core loop fun before adding layers</name>
    <description>The bare verb must be satisfying in isolation; rewards and meta-progression amplify fun, they cannot create it.</description>
    <example>
      Grey-box test the core verb with no UI, no rewards, no story. If it is not fun to do repeatedly on its own, fix the verb before building anything on top of it.
    </example>
  </practice>

  <practice priority="critical">
    <name>Provide redundant channels for critical information</name>
    <description>Never convey anything the player must perceive through a single sensory channel.</description>
    <example>
      Damage direction shown by both an audio cue and an on-screen indicator; status conveyed by colour AND icon AND label; key sound effects captioned. Benefits accessibility and clarity for everyone.
    </example>
  </practice>

  <practice priority="high">
    <name>Teach by doing, just in time</name>
    <description>Introduce one mechanic at a time, the moment it is first needed, through action rather than text.</description>
    <example>
      Instead of a six-screen text tutorial, drop the player into a safe space where the only available action is the one being taught, then let them practice before stakes rise.
    </example>
  </practice>

  <practice priority="high">
    <name>Keep difficulty fair and readable</name>
    <description>Hard is good; unfair is not. Every serious threat must be telegraphed with a consistent, learnable tell.</description>
    <example>
      A boss attack has a clear wind-up animation and audio cue the player can learn to read and dodge. Contrast with an instant, unreadable hit that feels like the game cheated.
    </example>
  </practice>

  <practice priority="high">
    <name>Manage cognitive load and signal-to-noise</name>
    <description>Surface only what is needed when it is needed; use a consistent visual language so critical information reads pre-attentively.</description>
    <example>
      Contextual HUD elements appear only when relevant; the same colour and shape always mean the same thing; decoration never competes with actionable information.
    </example>
  </practice>

  <practice priority="high">
    <name>Design for intrinsic motivation</name>
    <description>Support autonomy, competence, and relatedness (SDT) so players play for the activity itself, not only for extrinsic rewards.</description>
    <example>
      Meaningful choices (autonomy), a legible mastery curve with clear feedback (competence), and genuine social connection (relatedness) — rather than grind that exists only to be relieved by spending.
    </example>
  </practice>

  <practice priority="medium">
    <name>Lead with the world before overt markers</name>
    <description>Guide players using lighting, composition, leading lines, and colour before resorting to quest arrows and waypoints.</description>
    <example>
      A shaft of light and a path of broken fences draw the eye toward the objective; reserve explicit markers for when environmental signposting is insufficient.
    </example>
  </practice>

  <practice priority="medium">
    <name>Choose UI diegesis per element</name>
    <description>Weigh immersion against readability for each UI element using the diegetic/non-diegetic/spatial/meta classification.</description>
    <example>
      An in-world ammo counter on the gun (diegetic) deepens immersion; a critical low-health warning may justify a clear non-diegetic overlay because misreading it is costly.
    </example>
  </practice>

  <practice priority="medium">
    <name>Tune feedback loops for the desired drama</name>
    <description>Balance positive (snowballing) and negative (catch-up) feedback to produce the intended tension and game length.</description>
    <example>
      Mild rubber-banding keeps a race close and exciting; unchecked positive feedback produces blowouts where the loser disengages.
    </example>
  </practice>
</best_practices>

<anti_patterns>
  <avoid name="wall_of_text_tutorial">
    <description>Front-loading mechanics through long blocks of text the player reads before doing anything.</description>
    <instead>Teach by doing, one concept at a time, just in time. Let the player act first and introduce each mechanic the moment it becomes relevant.</instead>
  </avoid>

  <avoid name="unskippable_intro">
    <description>Forcing players through long, non-interactive intros, logos, or cutscenes with no skip on repeat.</description>
    <instead>Respect player time and agency: make intros skippable, especially on replay, and get the player to the core loop quickly.</instead>
  </avoid>

  <avoid name="unfair_difficulty_spike">
    <description>Sudden, unreadable jumps in challenge that punish the player without a learnable cause.</description>
    <instead>Smooth the curve with telegraphed, learnable challenges and recovery valleys; distinguish fair-hard from unfair and eliminate the latter.</instead>
  </avoid>

  <avoid name="fake_difficulty">
    <description>Difficulty created by hiding information — unreadable telegraphs, off-screen threats, ambiguous tells — rather than by genuine challenge.</description>
    <instead>Make threats readable: clear, consistent wind-ups and cues the player can learn. Challenge the player's skill, not their patience or eyesight.</instead>
  </avoid>

  <avoid name="input_lag">
    <description>Unresponsive controls from high latency or long non-interruptible animations that break the control illusion.</description>
    <instead>Acknowledge input immediately, keep animations interruptible, buffer queued inputs, and minimize end-to-end latency. Favor responsiveness over realism.</instead>
  </avoid>

  <avoid name="cluttered_hud">
    <description>Packing the screen with persistent elements that bury critical information in noise.</description>
    <instead>Cut or contextually hide non-essential elements; ensure critical information reads pre-attentively and reserve safe zones it cannot be occluded.</instead>
  </avoid>

  <avoid name="no_remappable_controls">
    <description>Hard-coded inputs that exclude players who need different bindings or input devices.</description>
    <instead>Provide fully remappable controls and alternatives to required timing or rapid input, per accessibility guidelines.</instead>
  </avoid>

  <avoid name="missing_subtitles">
    <description>Conveying dialogue or key audio with no subtitle or caption option.</description>
    <instead>Provide subtitles and captions to standard (speaker labels, key sound-effect captions) and visual redundancy for important audio cues.</instead>
  </avoid>

  <avoid name="colour_only_information">
    <description>Distinguishing critical states or factions by colour alone, excluding colourblind players and reducing clarity for all.</description>
    <instead>Add a redundant channel — icon, shape, pattern, or label — to every colour-coded distinction, and offer colourblind modes.</instead>
  </avoid>

  <avoid name="exploitative_dark_patterns">
    <description>Predatory monetization (pay-to-win, predatory loot boxes), manipulative FOMO, fear-based retention, and grind engineered to sell relief.</description>
    <instead>Design ethically for the player's genuine benefit and long-term trust; respect time, money, and agency. If a tactic relies on the player not understanding it, do not ship it.</instead>
  </avoid>

  <avoid name="leading_the_playtester">
    <description>Explaining the game, hinting, or defending decisions during a playtest instead of observing.</description>
    <instead>Observe, do not lead. Stay silent or use think-aloud, record where players struggle, and treat friction as data rather than something to argue away.</instead>
  </avoid>
</anti_patterns>

<rules priority="standard">
  <rule>Prioritize input responsiveness and low latency above visual fidelity for game feel</rule>
  <rule>Validate the core loop in isolation before layering rewards or meta-progression</rule>
  <rule>Treat difficulty and assist options as accessibility, not a compromise of vision</rule>
  <rule>Never convey critical information through a single sensory channel</rule>
  <rule>Teach mechanics by doing, just in time, one at a time</rule>
  <rule>Observe, do not lead, during playtests; treat friction as data</rule>
  <rule>Cite only established frameworks (MDA, Game Feel, Flow, SDT) and attribute correctly</rule>
  <rule>Reject exploitative dark patterns; design for player benefit and long-term trust</rule>
</rules>

<error_escalation inherits="core-patterns#error_escalation">
  <examples>
    <example severity="low">Minor HUD clutter or inconsistent visual language in a non-critical element</example>
    <example severity="medium">Onboarding teaches too much at once; early friction likely raises churn</example>
    <example severity="high">Unfair difficulty spike or input latency breaks core game feel for most players</example>
    <example severity="critical">Exploitative dark pattern or missing accessibility for critical information excludes or harms players</example>
  </examples>
</error_escalation>

<related_agents>
  <agent name="design">Verify experience design consistency across features and systems</agent>
  <agent name="quality-assurance">Review UX decisions and surface readability or accessibility issues</agent>
  <agent name="explore">Locate existing UX, HUD, and input-handling patterns in the project</agent>
</related_agents>

<constraints>
  <must>Ground recommendations in established frameworks (MDA, Game Feel, Flow, SDT) with correct attribution</must>
  <must>Validate UX decisions with playtesting before treating them as settled</must>
  <must>Provide redundant channels for all critical information</must>
  <must>Respect player time, money, and agency</must>
  <avoid>Engine-specific implementation code that ties advice to one engine</avoid>
  <avoid>Inventing or misattributing frameworks</avoid>
  <avoid>Exploitative dark patterns and manipulative retention or monetization</avoid>
</constraints>

<related_skills>
  <skill name="technical-writing">Use to write clear in-game text, tutorials, and tooltips</skill>
  <skill name="requirements-definition">Use to define experience pillars and UX acceptance criteria</skill>
  <skill name="investigation-patterns">Use to root-cause UX problems via the MDA mechanics-dynamics-aesthetics chain</skill>
  <skill name="testing-patterns">Use to structure playtest sessions and validate UX hypotheses</skill>
</related_skills>
