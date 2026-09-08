# SPIKE-001 Findings

## Command

```text
cabal build all
cabal exec -- runghc -ilibs/pattern/src -ilibs/subject/src -ilibs/gram/src \
  design/spikes/SPIKE-001-frame-registry/scripts/Main.hs
```

The first command is required in a fresh worktree to populate its local Cabal package
database. The driver completed with 17 PASS assertions.

`cabal test all` also passes. Five Gram corpus checks are pending because
`libs/gram/test-data/tree-sitter-gram` is not initialized; the pending checks are unrelated
to this documentation-and-spike-only change.

## Observations

| Experiment | Result | Finding |
|---|---|---|
| Gram fixture parsing | Pass | `Gram.fromGramWithIds` accepts the aircraft/maintenance, ambiguous-reference, and repair-plan fixtures. |
| Registry flattening | Pass | Recursive Definitions flatten into one Frame-local member map; ordered local identities preserve containment links. |
| Indirect cycle | Pass | `engine -> fuel-system -> fuel-pump -> diagnostic-procedure -> engine` is finite in the registry and creates no recursive copies. |
| Anonymous definition | Pass | Admission assigns `#spike-1` before reference resolution; its identity is Frame-local. |
| Direct self-reference | Pass | Admission rejects a Member whose element list contains its own local identity. |
| Raw Pattern import | Pass | Atomic and fuller occurrences of the same identity are ambiguous after Gram transformation and must fail instead of selecting a meaning. |
| Pattern.Reconcile | Pass | A Merge policy reconciles duplicate `fuel-pump` Subjects. `reconcile` accepts one Pattern, so Additive requires Frame code to prepare an adapter input. |
| Replace policy | Source-confirmed | `LastWriteWins` and `FirstWriteWins` select a Subject value but both use `UnionElements`; Frame Replace cannot directly delegate to either. |
| Span validation | Pass | Pair endpoints resolve against the Span's ordered left/right Frame identities; missing endpoints are rejected. |
| Deletion protection | Pass | Local reference removal and incident Bundle-pair dangling endpoints both reject Frame replacement. |
| Pair rebind | Pass | `rebindPair` validates a new ordered endpoint pair before permitting the Frame replacement. |
| Import/rebase + Attach | Pass | A default identity collision rejects; an explicit collision-avoiding map copies the reachable closure and Attach preserves local closure. |

## Failure Cases

- The initial command used `-i libs/pattern/src`; `runghc` interpreted the separated path as
  a target. Attached flags (`-ilibs/pattern/src`) are required.
- Running `cabal exec` before `cabal build all` in the new worktree failed because its
  `dist-newstyle/packagedb` did not yet exist.

## Implications

- Frame admission needs a CST-preserving `PatternLike` boundary. Raw `Pattern Subject`
  import can be compatibility-only and must surface ambiguity.
- Frame `Replace` and `Additive` remain distinct from generic `ReconciliationPolicy`:
  Replace swaps validated registry state; Additive supplies a prepared Pattern adapter to
  `Pattern.Reconcile`.
- FrameSpace centrally owns cross-Frame referential integrity. A future incident-Span index
  is a performance optimization, not a requirement for the semantic model.
- Registry-backed `find`, `containers`, `siblings`, and `framePara` remain an explicit
  follow-up before implementation planning.
