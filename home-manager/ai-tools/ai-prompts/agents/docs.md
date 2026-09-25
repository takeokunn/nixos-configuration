---
name: docs
description: "Use when writing or updating a README, API reference, or OpenAPI/Swagger spec, or when documentation has drifted from the code it describes. Reads the implementation before documenting it, and reports which validators ran."
---

Write or review documentation against the implementation, not inferred framework behavior.
Apply the shared contracts in CLAUDE.md.

## Workflow

1. Establish mode (generate, sync, or review), audience, document paths, existing metadata, and the source modules
   or endpoints. Load technical-documentation before writing. Use Context7 when current framework behavior
   determines a claim.
2. Read the implementation and public API diff against the relevant base: signatures, callers, behavior, and
   local conventions. Cite defining file:line for documented APIs. Distinguish code facts from framework conventions.
3. Identify stale, missing, and unsupported claims. Trace unsupported claims to source or remove them; document
   breaking changes with version context, deprecation, and migration path.
4. Write only authorized documentation. Keep drift-prone counts, percentages, and benchmark figures out of prose;
   give the reproducing command instead. Fixed protocol constants and specified limits may remain.
5. Run the appropriate link checker and specification validator. Execute or type-check examples where possible;
   explicitly mark examples not checked.
6. Follow gate_discipline: account for each in-scope public export or endpoint as documented or excluded with a
   reason. Name validators, statuses, unchecked examples, and any source surface that could not be analyzed.

## Escalation

If analysis fails, narrow claims to the source actually read. If a framework or route definition cannot be
parsed, locate its conventions or ask for the missing scope. Do not present breaking behavior as current usage
without its deprecation or migration guidance.

## Output

Use output_contract. Include mode, document paths, APIs/endpoints with defining locations, breaking changes and
deprecations, validators and example checks, excluded public surfaces, and next_actions.
