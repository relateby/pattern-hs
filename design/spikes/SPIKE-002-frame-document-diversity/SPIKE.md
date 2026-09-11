---
id: SPIKE-002
title: Frame Admission Against Found Documents
status: done
rfcs: [RFC-001]
created: 2026-09-11
---

# SPIKE-002: Frame Admission Against Found Documents

## 1. Question

Does `PatternLike` admission and Frame registry closure — as described in
`design/rfc/comments/RFC-001-managed-frame-containers.md` after the flat-ordinal
positional-address redesign — hold up against Gram documents nobody wrote for this model?
SPIKE-001 validated feasibility against a hand-designed aircraft/maintenance domain built
with the registry model already in mind. This spike instead ingests documents authored for
other purposes — a separate project's own example corpus, and this repo's adversarial
parser fixtures — as single Frames, to surface admission or closure behavior the
hand-designed exercise had no reason to hit.

Scope is deliberately narrow: Frame admission and closure only. No Span, no FrameSpace, no
`correlateByIdentity`. Cross-document correlation and incident-Span cost remain a later
spike's question.

This informs, but does not resolve, the Frame-only half of Open Question 5 (does
closure/registry validation scale on a large or deeply nested document) and general
confidence in the ordinal redesign (§Identity and addresses) ahead of the ADR.

## 2. Method

`scripts/Main.hs` parses each source document with `Gram.fromGram` — not
`fromGramWithIds`, which synthesizes `#N` identities for anonymous subjects at parse time
and so bakes in exactly the generated-identity model this RFC superseded; `fromGram`
preserves true anonymity (`Symbol ""`) so admission assigns the ordinal itself — admits the
result into a single empty Frame using the content-based recovery rule from §Defining and
reference occurrences (identity alone is a reference-candidate; any labels, properties, or
elements make it a definition), and checks the outcome: does it parse, does it admit
without error, and does closure hold.

Five source documents, none authored for this model:

- `libs/gram/test-data/tree-sitter-gram/examples/data/social.gram` — real named-member
  reuse: a `graphistas` group container references five previously defined `Person`
  members by name, and three anonymous `KNOWS` relationship-Patterns reference named
  endpoints. Exercises named-reference resolution and content-based definition/reference
  recovery on relationship-shaped occurrences.
- `libs/gram/test-data/tree-sitter-gram/examples/data/route-66.gram` — a real, substantial
  (82 non-comment lines) document: ~18 labeled, propertied `Junction` members, plus one
  `Segment` container whose Bundle-shaped element list holds 11 nested, labeled, propertied
  relationship-Patterns chaining those Junctions in sequence. Exercises registry scale and
  a deeply nested single admission batch with many named forward references.
- `libs/gram/test-data/roundtrip/custom/anonymous-subject.gram` — a single anonymous,
  labeled, propertied atomic Pattern. Exercises the ordinal-assignment path directly.
- `libs/gram/test-data/roundtrip/custom/implicit-root.gram` — `a-b-c`, an implicit-root
  chain. Exercises admission of Gram's path shorthand.
- `libs/gram/test-data/roundtrip/custom/deep-nesting.gram` — `[a | b | c | d | e]`, nested
  containment five deep. Exercises registry flattening under real nesting depth, however
  shallow in absolute terms.

Run from the repository root:

```text
cabal build all
cabal exec -- runghc -ilibs/pattern/src -ilibs/subject/src -ilibs/gram/src \
  design/spikes/SPIKE-002-frame-document-diversity/scripts/Main.hs
```

## 3. Time-box

One day.

## 4. Notes

### 2026-09-11

- First driver draft used `fromGramWithIds` and a two-pass admission function that
  re-walked children to recompute their addresses after admitting them. That re-walk
  cannot recover an anonymous child's real assigned ordinal (ordinals are assigned by
  threading state through admission, not derivable from the Pattern alone), so it silently
  skipped closure validation for exactly the anonymous-relationship shape (`(a)-[:R]-(b)`)
  this spike most needed to exercise. Rewritten as one pass: `admitPattern` returns the
  address a node resolves to, threaded directly into its parent's element list.
- Switched from `fromGramWithIds` to `fromGram` after route-66.gram's registry came back
  keyed `Named "#1"` .. `Named "#12"` for what should have been anonymous relationship
  Patterns — `fromGramWithIds` assigns those at parse time, pre-empting Frame admission's
  own ordinal assignment. See Findings.
- `cabal build all` required before `cabal exec -- runghc`, same as SPIKE-001.
- Initial parse-failure findings for `social.gram`, `implicit-root.gram`, and
  `deep-nesting.gram` were checked only against `gramref`, this repo's own reference CLI —
  not authoritative. Re-checked against the canonical `gram check`
  ([gram-data/tree-sitter-gram/tools/gram](https://github.com/gram-data/tree-sitter-gram/tree/main/tools/gram)):
  all three fail identically there too, so `Gram.Parse` was never the problem. Issue #75
  closed as a misdiagnosis; #76 retitled from "parser gap" to "invalid fixtures." See
  Findings below for the corrected account.

## 5. Findings

Ordinal addressing and closure validated cleanly against real, substantial, independently
authored content: `route-66.gram` (82 non-comment lines from a separate project's own
example corpus, not written for this model) admitted into a 26-entry registry — 14 named
Junction/Segment members plus 12 anonymous `Route`-labeled relationship-Patterns, each
correctly assigned a flat positional ordinal and each correctly resolving its two named
endpoints — with closure holding across the whole document on the first attempt, no
special-casing. `anonymous-subject.gram` admitted its one anonymous, labeled, propertied
Pattern to a single `Positional 1` entry, confirming the ordinal model — not the
generated-identity one — against real parser output rather than a hand-constructed example.

Three fixtures failed before reaching Frame admission at all, each a genuine, unanticipated
finding rather than a toy-example near-miss:

- **`fromGramWithIds` conflicts with the RFC's own model.** It synthesizes `#N` symbols for
  anonymous subjects at parse time; the RFC requires anonymous subjects to acquire no
  invented Subject identity at all (§Identity and addresses). Using it produced `Named "#1"`
  registry entries — exactly the superseded generated-identity model SPIKE-001 validated
  and this RFC later replaced. `fromGram` is the correct entry point for any raw-Pattern
  admission path; `fromGramWithIds` is not a compatibility option for it.
- **`social.gram`, `implicit-root.gram`, and `deep-nesting.gram` do not parse under
  `Gram.Parse`** — but `Gram.Parse` is not at fault. `gramref`, this repo's own reference
  CLI, is not the authoritative implementation; the canonical tool is `gram check` from
  [gram-data/tree-sitter-gram/tools/gram](https://github.com/gram-data/tree-sitter-gram/tree/main/tools/gram),
  built from the actual grammar. Checked against it directly, all three fail identically
  (`route-66.gram` passes clean as a positive control), so `Gram.Parse` correctly agrees
  with the canonical grammar in every case — there is no pattern-hs parser gap here at all.
  `social.gram`'s top-level comma-concatenation (`(ee:Person {...}), (mh:Person {...})`) is
  invalid per the canonical grammar despite the file's own inline comment claiming it works
  — a stale example in `gram-data/tree-sitter-gram`'s own upstream corpus, not a pattern-hs
  issue; closed as
  [relateby/pattern-hs#75](https://github.com/relateby/pattern-hs/issues/75) once confirmed,
  to be reported upstream separately. `implicit-root.gram` (`a-b-c`) and `deep-nesting.gram`
  (`[a | b | c | d | e]`) use syntax that was never valid gram at all, canonically — these
  are this repo's own fixtures and are simply wrong, not aspirational; tracked as
  [relateby/pattern-hs#76](https://github.com/relateby/pattern-hs/issues/76), retitled after
  the correction. Investigating the second of those surfaced a genuinely independent bug:
  `RoundtripSpec.hs`'s "Custom Edge Case Roundtrip Tests" reports *"No .gram custom test
  files found in `libs/gram/test-data/roundtrip/custom/`"* and skips as pending when run via
  `cabal test`/`cabal test all`, even though five files are present — `findCorpusFiles`'s
  repo-root-relative path only resolves when the test binary's working directory is the
  repo root, which `cabal test` does not use. Running the built `gram-test` binary directly
  from the repo root finds all five files and correctly reproduces both fixtures' failures
  (2 of 6 examples). Filed as
  [relateby/pattern-hs#77](https://github.com/relateby/pattern-hs/issues/77), unaffected by
  the correction above. All three are test-fixture/test-infrastructure issues, out of this
  spike's Frame-only scope, and are not fixed here.

## 6. Conclusion

Frame admission and closure, under the corrected flat-ordinal model, held up against real,
independently authored content with no special-casing required — the positive result this
spike was timeboxed to obtain. `Gram.Parse` itself came out of this spike looking better
than the first pass of findings suggested: checked against the canonical `gram` tool, it
agrees with the grammar on every fixture tried, including the three that failed to parse.
The real findings were two invalid fixtures — one upstream, one this repo's own — and a
real, independent `cabal test` discovery bug, all worth a follow-up outside this spike's
scope but silent on the Frame/Span model's soundness. `fromGramWithIds` should not be
assumed a valid stand-in for `fromGram` anywhere the RFC's anonymous-identity model
matters — a note worth carrying into the eventual ADR's admission-adapter section
alongside SPIKE-001's own `PatternLike`-adapter conclusion.
