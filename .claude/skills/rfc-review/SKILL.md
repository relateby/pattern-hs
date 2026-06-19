---
name: rfc-review
description: Review a draft RFC for scope, clarity, and contributor-readiness. Use before opening an RFC for comment or merging a proposal branch. Produces concrete suggested revisions, not just a report.
---

# RFC Review

## Overview
Use this skill to evaluate a draft RFC against a consistent set of quality criteria before it is opened for comment. Works for self-review by the author, maintainer review during a PR, or peer feedback from a collaborator. Produces specific, actionable suggested revisions — not a checklist of observations.

## Workflow
1. Read the RFC in `proposals/rfc/`.
2. Read `proposals/rfc/README.md` to confirm structural compliance.
3. Evaluate the RFC against each criterion below.
4. For each criterion that fails or is weak, draft a specific suggested revision — quote the problematic text and propose replacement text or a concrete addition.
5. Summarize findings: what is strong, what needs revision, and what is blocking vs. advisory.

## Review Criteria

### 1. Scope fit
Is the RFC the right size for its stated purpose?
- A PoC RFC should specify layout and boundaries, not implementation detail.
- A feature RFC should specify behavior and interfaces, not code.
- A subsystem RFC should specify contracts and failure modes, not algorithms.

**Flag:** RFC specifies code, method signatures, or implementation detail that belongs in the implementation, not the proposal.  
**Flag:** RFC is so vague that an implementer would face constant ambiguity about what was agreed.

### 2. Acceptance criteria anchored in behavior
Can a contributor tell when the work is done by observing something, not by checking off packages created?
- Acceptance criteria should describe what a user, agent, or system *does* and *sees*.
- Package structure and file layout are not acceptance criteria.

**Flag:** No user stories, demo scenario, or observable acceptance criteria present.  
**Flag:** Acceptance criteria describe structure ("the registry package exists") rather than behavior ("a ghost can register and receive a token").

### 3. Demo scenario
Is there a concrete path a contributor follows to verify the work?
- Should include: what to run, in what order, and what to observe.
- Should be completable by a new contributor within a stated time (e.g. 15 minutes).

**Flag:** No demo scenario or "getting started" path described.  
**Flag:** Demo scenario assumes knowledge not present in the RFC or repo README.

### 4. Implementation readiness
Is there enough information for implementation to begin without another round of design?
- Key interfaces, tool names, or API shapes should be named (not fully specified).
- Open questions should be listed explicitly, not hidden.
- Dependencies between packages should be clear.

**Flag:** A contributor would need to make significant design decisions not acknowledged as open questions.  
**Flag:** Open questions are present but not listed — they are buried in prose or implicit.

### 5. Over-specification
Is the RFC constraining implementation detail that should be left to the implementer?
- Method signatures, data structure internals, and algorithm choices belong in code.
- Interface names and tool names are appropriate to specify; their implementations are not.

**Flag:** Code blocks present that specify implementation rather than illustrate intent.  
**Flag:** RFC would be wrong if an implementer chose a different variable name or data structure.

### 6. Alternatives
Are the alternatives realistic comparisons that help a reader understand why this approach was chosen?
- Each alternative should name a real option that was genuinely considered.
- Rejection reasons should be specific to this project, not generic.

**Flag:** Alternatives are strawmen that no contributor would actually propose.  
**Flag:** A significant alternative is missing — one a reviewer would ask "why not X?" about.

### 7. Consistency with architecture
Does the RFC align with decided components and open questions in `docs/architecture.md`?
- It should not silently contradict decided technology choices.
- If it resolves an open question, it should say so explicitly.
- If it challenges a decided component, it should reference or propose an ADR.

**Flag:** RFC introduces a technology or pattern inconsistent with `docs/architecture.md` without explanation.

## Output Format

```
## RFC Review: [RFC title]

### Summary
[2-3 sentences: overall assessment and whether it is ready to open for comment]

### Blocking Issues
[Issues that must be resolved before the RFC is ready. For each: criterion, problem, suggested revision.]

### Advisory Issues
[Issues worth addressing but not blocking. Same format.]

### Strengths
[What is working well and should be preserved.]
```

For each blocking or advisory issue, include:
- **Criterion:** which of the seven criteria applies
- **Problem:** what is wrong or missing, with a quote if relevant
- **Suggestion:** specific text to add, remove, or replace
