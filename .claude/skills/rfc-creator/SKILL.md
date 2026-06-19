---
name: rfc-creator
description: Create or update Requests for Comment for this repository. Use when the user wants a new feature, subsystem, module, game mechanic, API change, or vendor contribution proposal drafted in `proposals/rfc/` before implementation.
---

# RFC Creator

## Overview
Use this skill to draft repository-specific RFCs that follow `proposals/rfc/README.md` exactly. RFCs are for proposals before implementation, not for settled architectural decisions.

## Workflow
1. Read `proposals/rfc/README.md` and any relevant context from `README.md`, `docs/project-overview.md`, and `docs/architecture.md`.
2. Confirm the change belongs in an RFC:
   - new subsystem, module, or feature
   - significant behavior or API change
   - vendor contribution introducing new interfaces or behavior
3. Determine the next RFC number from the index in `proposals/rfc/README.md` (highest existing `RFC-NNN` + 1). Filename is `RFC-NNN-short-title.md` — `NNN` zero-padded to three digits, `short-title` in lowercase kebab-case (e.g. `RFC-011-codec-persistence.md`).
4. Create a branch named `proposal/RFC-NNN-short-title` matching the RFC filename exactly before writing any files. Do not proceed without switching to this branch.
5. Create or update `proposals/rfc/RFC-NNN-short-title.md` on that branch.
6. Use this structure exactly. The five `##` sections are required and in this order; the header fields below `**Date:**` are optional — include the ones that apply, omit the rest:

```markdown
# RFC-NNN: Title

**Status:** draft | under review | accepted | rejected
**Date:** YYYY-MM-DD
**Authors:** @handle
**Repository:** [github.com/relateby/pattern-hs](https://github.com/relateby/pattern-hs)
**Supersedes:** prior proposal/doc this RFC absorbs (note it as removed)
**Depends on:** RFC-NNN (one-line reason), ...
**Followed by:** what comes after this RFC lands (e.g. Rust port, CLI surface)
**Related modules:** `Module.One`, `Module.Two`

## Summary

## Motivation

## Design

## Open Questions

## Alternatives
```

## Writing Rules
- Keep `## Summary` to one clear paragraph that states the device/change and how it relates to existing RFCs.
- Lead `## Motivation` with the problem; use named sub-sections (e.g. "Why not X?", "Use Cases") when it sharpens the case.
- Put interfaces, flows, and pseudocode in `## Design` when useful. Common `## Design` sub-sections in this repo: a named **Conventions** subsection when invertibility/round-tripping relies on encoded metadata, an **Implementation Sequence** (numbered, phased), and a **port** step (`pattern-rs` / TypeScript) when the design is meant to be ported.
- Use `## Open Questions` to surface unresolved choices instead of hiding them; number them.
- Compare realistic alternatives, not strawmen, and state why each is rejected.
- Keep the proposal consistent with the current architecture unless the RFC explicitly challenges it.

## Output
When creating a new RFC:
1. Confirm the active branch is `proposal/RFC-NNN-short-title` before writing files.
2. Write the RFC file under `proposals/rfc/`.
3. Update `proposals/rfc/README.md`: add the RFC to the appropriate category section's index table (or create a new section if it opens a new area), and reflect any new dependency in the Implementation Order diagram.
4. Call out missing inputs or unresolved dependencies.
5. Remind the author to run `/rfc-review` before opening the proposal for comment.
