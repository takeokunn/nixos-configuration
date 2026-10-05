---
name: formal-methods
description: Use when a claim about permissions, configuration constraints, state transitions, concurrency, or an algorithm needs a solver or model checker rather than more example tests, or when an existing model has drifted from its spec or code. Covers choosing among Z3, Alloy, Quint, TLA+, and Lean, minimal models, and reproducing counterexamples as tests.
metadata:
  version: "1.0.0"
---

# Formal methods

The language model drafts the model, repairs it, and explains the result. The solver, model checker, or proof
assistant decides.

Use this when example tests cannot cover the space: interleavings of retries and crashes, combinations of roles
and resources, or a rule set that may contain a contradiction or a rule no input reaches. When the property is
executable code over generated inputs, a property-based test from
[testing-patterns](../testing-patterns/SKILL.md) is cheaper.

## First model

1. **Pick the source of truth.** A trusted spec, ADR, or API contract states the expected behavior, and the code
   is compared against it. Without one, the code's behavior describes what happens, not what is correct. When
   spec and code disagree, the outcome is a question for the domain owner, not a choice made here.
2. **Extract claims before choosing a tool.** Phrase each as allowed, forbidden, eventually happens, never
   happens, reachable, or preserves an invariant. State empty, error, timeout, retry, and crash behavior
   explicitly, since models and code most often part there.
3. **Choose the smallest tool that can produce a counterexample.**

   | Shape of the question | Tool |
   |---|---|
   | Consistency of predicates or constraints over finite values: config validation, overlapping rules, flag combinations | Z3 |
   | Relations among users, roles, resources, ownership, and tenants | Alloy 6 |
   | State transitions and interleavings: retries, queues, caches, crash recovery, protocols | Quint, or TLA+ with TLC |
   | A theorem over unbounded inputs, or an algorithm's correctness | Lean 4 |

   Z3 does not model interleavings, TLA+ costs more than a single predicate needs, and Lean's feedback loop is
   too slow for hunting a configuration bug.
4. **Build the minimal model.** Drop I/O, frameworks, storage, and UI unless they define the property. Model only
   the state, actions, relations, and invariants the claim needs. Keep domains small and state the bounds: a pass
   at three users and two resources says nothing about larger instances.
5. **Show that the check can fail.** Add a case the model must accept, because an over-constrained model
   satisfies every safety property without exercising it. Add a deliberately broken variant the checker must
   reject. This is the model-level form of test-integrity's
   [non-vacuity audit](../test-integrity/SKILL.md#the-non-vacuity-audit).
6. **Iterate on verifier output.** Repair syntax and modeling errors first. Weakening a property or shrinking a
   bound to reach a pass is the weakened verification `hard_rules` forbids. When the domain owner changes the
   property, record that decision and rerun from the new property.
7. **Reproduce the counterexample in the implementation.** A counterexample is a claim about the model until a
   test forces the same input, relation instance, or interleaving on the code, using barriers, injected crashes,
   or a fixed clock. If the test fails the same way, the bug is real and the test becomes its regression guard.
   Run the corrected model's scenario against the code as well, because a passing model is also only a claim
   about the model. When model and code disagree, suspect the model's assumptions first, such as isolation
   level, lock semantics, or retry policy, and rerun both.
8. **Report in domain terms.** Say who can do what, which ordering is accepted, which configuration no input
   reaches, or which crash sequence loses data. A bare sat, unsat, or trace is not the result. Label each result
   as described under [Result labels](#result-labels).

## Keeping a model aligned

- Give each claim an identifier and record where it lives in the spec, the code, and the model, so a change on
  one side names the claims to recheck.
- Assign each mismatch one primary category. Choose it by which side departs from the agreed domain rule, not by
  which side changed first. When no rule has been agreed, the category is decision.

  | Category | What departed |
  |---|---|
  | spec | The documentation no longer states the agreed rule |
  | code | The implementation departs from the agreed rule |
  | model | The abstraction no longer matches the rule or the code's relevant behavior |
  | harness | Tool version, CI wiring, or parser changed while the property did not |
  | decision | The domain owner has not settled which behavior is intended |
  | coverage | A claim has no model, or the model omits a path the code now has |

- Repair harness drift without touching the property or its bounds.
- Keep each check's expected result, a pass or a specific counterexample, in the repository next to the model.
  Run the check in CI when a path that feeds it changes.
- Do not claim that the code refines the model unless a check established it.

## Result labels

These refine CLAUDE.md's evidence tiers for verifier output.

| Label | Meaning | Evidence tier |
|---|---|---|
| machine-confirmed | A verifier ran in this session; report the command, bounds, and outcome | verified |
| log-confirmed | Taken from a cited CI or earlier run log | verified |
| diff-inferred | Reasoned from a change or derived by hand, without running a verifier | inferred |
| not-run | Planned only; no result is claimed | assumed |

## Running the tools

The tools in the table are installed on hosts that import `home-manager/development/advanced.nix`. Elsewhere,
use `nix shell nixpkgs#<attribute>` with `z3`, `alloy6`, `quint`, or `tlaplus`. Lean 4 is installed nowhere by
this configuration; obtain it with `nix shell nixpkgs#lean4` and check a file with `lean File.lean`.

| Tool | Command | Reading the result |
|---|---|---|
| Z3 | `z3 model.smt2` | `sat` means a satisfying assignment exists; `unsat` means none does |
| Alloy 6 | `alloy6 exec -f -o <outdir> model.als` | For a `check` command, SAT means a counterexample was found |
| Quint | `quint verify --backend tlc --invariant <name> spec.qnt` | TLC explores the whole finite state space and ignores `--max-steps`; exits nonzero on a violation |
| Quint | `quint run --invariant <name> spec.qnt` | Random simulation; a pass is a sample, not exhaustive |
| TLA+ | `tlc Spec.tla` with a `Spec.cfg` beside it | Reports the violated invariant with a trace |

`quint verify` defaults to the Apalache backend, which bounds traces by `--max-steps`. The nixpkgs build bundles
a pinned Apalache; a Quint installed outside Nix downloads it on first use, which needs the user's approval. Pass
`--backend tlc` when the state space is finite and an exhaustive answer is wanted. Run Quint from a scratch
directory, because `quint verify` writes an `_apalache-out/` directory into the working directory with either
backend.

## Related

Naming a skill here does not load it. Use the runtime's skill loader, or read its SKILL.md, when triggered.

- [testing-patterns](../testing-patterns/SKILL.md): property-based tests when the property is executable code
- [test-integrity](../test-integrity/SKILL.md): whether a green check could have failed
- [investigation-patterns](../investigation-patterns/SKILL.md): tracing a reproduced counterexample to its cause
