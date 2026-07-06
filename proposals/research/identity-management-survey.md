# Identity Management in Data Structures & Databases — A Survey

**Status**: 📚 Research companion
**Purpose**: Inform a future RFC on *scoped identity namespaces* for `Pattern` (the prerequisite gating RFC-011 implementation; see RFC-011 Open Question 3).
**Date**: 2026-07-03
**Method**: Fan-out web research (24 primary sources, 118 extracted claims, 25 adversarially verified 3-vote). Findings on RDF and Datomic were spec-verified against current W3C/Datomic docs; findings on Neo4j, content-addressing, UUIDv5, Matrix, XML, and Wikidata were fetched from primary sources but did not go through the formal verify pass — confidence is marked per section.

---

## The problem this survey serves

`Pattern` has text-authored `.gram` files whose elements carry identity as a bare string `Symbol`. Identities are either **human-chosen** (`n`, `alice`) or **anonymous** (the parser assigns sequential `#1`, `#2` *per document*). Multiple files are ingested into a shared store, where identity equality drives merge/reconciliation (RFC-010) and upsert (RFC-011).

Five requirements the design must satisfy:

1. **Clash-free ingest** — per-document id-spaces must not collide. `(n)` in `a.gram` and `(n)` in `b.gram` are different entities *by default*.
2. **Anonymous identity that travels** — `#1`/`#2` are minted per document; anonymous elements need stable, clash-free identity once they leave their document.
3. **Deliberate cross-document identification** — the opposite case must stay possible: the *same* entity legitimately appearing in multiple files, identified across them on purpose.
4. **Idempotent re-ingest** — loading the same file twice must upsert, not duplicate. Interacts hard with #2: sequential anonymous ids are order-dependent, so re-parse must mint the *same* identities.
5. **Frames as namespace boundaries** — RFC-011's `(frame_id, id)` composite key already treats Frame as scope; the design should formalize that, not fight it.

**Central tension:** requirements 1 and 4 pull against each other. Qualifying ids by something unique-per-ingest (timestamp, random scope id) gives clash-freedom but loses idempotency; qualifying by something deterministic (file path, content hash) gives idempotency but forces a decision about what happens when the file *changes*. Most prior art picks a side — RDF skolemization is per-ingest-unique, UUIDv5 is deterministic. Where each system lands on this axis is the most useful thing the survey reveals.

---

## 1. RDF — the theoretical frame (spec-verified)

RDF is the closest prior art: it *is* a text-first, file-oriented, merge-heavy system, and it splits identity into exactly gram's two cases.

- **IRIs = global identity.** "By design, IRIs have global scope. Thus, two different appearances of an IRI denote the same resource" ([RDF 1.1 Concepts §1.3](https://www.w3.org/TR/rdf11-concepts/)). Equality is strict codepoint comparison, no normalization (§3.2). Crucial precondition: this works *only* because authors mint identifiers under authority they control. Gram's bare `n` has no such authority — **gram symbols are not IRIs**, whatever they look like.

- **Blank nodes = document-scoped existentials.** This is precisely gram's `#1`/`#2`. Blank node identifiers are "always locally scoped to the file or RDF store, not persistent or portable" (Concepts §3.4); semantically they assert *existence*, not reference ([RDF 1.1 Semantics §5.1](https://www.w3.org/TR/rdf11-mt/)). Separate graphs never share blank nodes unless an enclosing structure (an RDF dataset) explicitly provides for it (§4.1). This is the spec-level root of the per-document clash (requirement 1).

- **Merge vs. union.** Semantics §4.1 formally distinguishes *merge* — blank nodes are "standardized apart" (renamed so they cannot collide) — from *union*, which preserves sharing. **Requirement 1 solved at spec level: multi-file ingest defaults to merge — rename-apart per document; union-like sharing only via an explicit enclosing structure.**

- **Skolemization** (Concepts §3.5) is the spec-blessed promotion of anonymous ids to stable global ones: replace blank nodes with minted globally-unique IRIs, conventionally under the IANA-registered `/.well-known/genid/` path under the publisher's domain. Semantically safe — `sk(G)` entails `G` and is entailment-equivalent for any graph not mentioning the new IRIs (Semantics §6) — but **irreversible** (`G` does not entail `sk(G)`), and it sacrifices the protective property that document-local ids cannot be redefined from outside. This is the direct model for promoting gram's `#N` ids to `(frame_id, #N)` store ids. (Design history: [Mallea/Arenas/Hogan/Polleres, ISWC 2011](https://marceloarenas.cl/publications/iswc11.pdf).)

- **The practice gap is instructive.** Implementations do *not* honor existential semantics — SPARQL, OWL, RIF, RDB2RDF all treat blank nodes as local constants (ISWC 2011). The round-tripping failure — a blank-node id returned from a query cannot be used to reference the same entity in the next query — forced Jena ARQ (`<_:b1>`) and Virtuoso (`<nodeID://b1>`) into non-standard, effectively-skolemized syntaxes. **Lesson: any anonymous id that escapes its document gets forced into de-facto stable identity anyway — skolemize deliberately at ingest rather than accidentally later.** Canonical Linked Data guidance ([Heath & Bizer](https://marceloarenas.cl/publications/iswc11.pdf)) files blank nodes under "RDF Features Best Avoided" and says name everything with URI references.

- **`owl:sameAs` is the cautionary tale for requirement 3.** Its semantics are total identity: the two URIs denote exactly the same thing, share all properties, symmetric and transitive ([OWL 2 Direct Semantics](https://www.w3.org/TR/owl2-direct-semantics/), [Primer](https://www.w3.org/TR/owl2-primer/)). Real-world usage "almost always violates" that — [Halpin/Herman/Hayes (W3C 2010)](https://www.w3.org/2009/12/rdf-ws/papers/ws21) catalog four weaker meanings actually in use (Same-Thing-But-Different-Context, Referentially-Opaque, Represents, Very-Similar-To). Because anyone can assert `sameAs` against your identifier without permission, transitive closures explode ([sameAs.cc](https://arxiv.org/abs/1907.10528): 558M asserted statements → ~35B under closure). **Proposed remedy: scope identity assertions to named graphs** — an equivalence holds within a context, not globally. Directly validates Frame-as-scope (requirement 5).

- **Empirical note:** anonymous identity is *pervasive* in published text-serialized graph data — a 2010 crawl found 57.8% of unique data-level terms were blank nodes (ISWC 2011), though with heavy sampling bias (one FOAF exporter dominated; per-domain average 7.5%); a 2012 follow-up found 25.7%. Cite as "blank nodes are common," not as a precise ratio. Implication: a text-first notation must treat anonymous elements as the common case, not an edge case.

## 2. Datomic — the operational ingest model (spec-verified)

Where RDF gives semantics, Datomic gives mechanics, and the analogy is near-exact with *document* substituted for *transaction*.

- **Entity ids** are database-unique (not globally unique), transactor-assigned, immutable — system identity kept strictly separate from domain identity ([Datomic: Identity and Uniqueness](https://docs.datomic.com/schema/identity.html)). This independently confirms RFC-011's "`StoreKey` is a subordinate physical locator" stance.

- **Tempids** are transaction-scoped local ids ([transaction data reference](https://docs.datomic.com/transactions/transaction-data-reference.html)). Every distinct tempid gets a permanent entity id at transaction time; repeated occurrences of the same tempid string *within* the transaction unify to one entity; and the transaction report returns a `:tempids` map so the caller learns what each local id became. **Requirements 1+2 as a working production system:** local symbol equality means sameness *within scope only*, resolution happens at the boundary, and the mapping is surfaced rather than hidden.

- **`:db.unique/identity` upsert:** if a tempid carries a unique-attribute value that already exists, it resolves to the *existing* entity instead of minting a new one. Re-transacting the same data adds nothing. **The only mechanism in the whole survey that natively solves idempotent re-ingest (req 4) and deliberate cross-document identity (req 3) with one device** — identity-as-key checked at ingest, rather than identity-as-assertion inferred afterward. (`:db.unique/value` does *not* upsert; conflicting upserts to two entities throw.)

- **Lookup refs** (`[attribute value]` pairs) elevate application-managed domain keys to first-class handles usable interchangeably with entity ids across the API, expanding to the entity id at transaction time and throwing if nothing matches ([lookup refs](https://blog.datomic.com/2014/02/datomic-lookup-refs.html)) — strict exists-or-throw semantics, deliberately distinct from upsert's create-or-match.

## 3. Property-graph practice — Neo4j (fetched, not formally verified)

- `elementId()` is documented as unsafe beyond a single transaction ([CDC docs](https://neo4j.com/docs/cdc/current/procedures/elementids-key-properties/)) — internal ids are never identity of record. Independently confirms RFC-011.
- `neo4j-admin import` assumes ids are globally unique across input files and offers **id-spaces (id-groups)** for per-source local id-spaces — a direct industrial precedent for "namespace assignment at ingest" — hard-failing on duplicates within a group ([import tutorial](https://neo4j.com/docs/operations-manual/current/tutorial/neo4j-admin-import/)).
- Official ingest guidance: create uniqueness constraints on application keys *before* importing; idempotency is enforced at the schema layer (`MERGE` on a key), not by the tool ([LOAD CSV](https://neo4j.com/docs/cypher-manual/current/clauses/load-csv/)).
- Composite node key constraints ([constraint syntax](https://neo4j.com/docs/cypher-manual/current/constraints/syntax/)) are the `(frame_id, local_id)` shape.

## 4. Content-addressed identity — Git, Unison, IPFS, Perkeep (fetched)

Identity = hash of content: deterministic and idempotent by construction, but conflates identity with content, so it breaks for *mutable entities* — change a property and it is a "different" thing. Unison's refinement (names are metadata over content hashes; [the big idea](https://www.unison-lang.org/docs/the-big-idea/)) works only because code definitions are immutable values. **Perkeep's permanode is the canonical fix**: a stable random anchor object whose hash is the identity, with mutations expressed as signed claims referencing it ([permanode schema](https://perkeep.org/doc/schema/permanode)). Relevance to us: gram subjects are *mutable* entities, so pure content-addressing is the wrong identity for **subjects** — but may be exactly right for identifying **documents / ingest events** (requirement 4).

## 5. Deterministic id generation — UUIDv5 (fetched; RFC 9562)

Name-based UUIDs hash `(namespace, name)` → identical inputs always yield the identical UUID ([RFC 9562](https://datatracker.ietf.org/doc/html/rfc9562), May 2024, obsoletes RFC 4122). The standard tool for making skolemization *deterministic*: `uuid5(file-scope-uuid, local-id)` gives anonymous elements identity stable across re-parses without coordination. Trade-off: determinism is only as stable as the *name* — if anonymous ids are ordinal (`#1` = "first anonymous element"), editing the file reorders them and the derived identities shift.

## 6. Scoped human-readable naming — Matrix, XML namespaces (fetched)

Matrix ids (`@localpart:server.name`; [spec appendices](https://spec.matrix.org/v1.3/appendices/)) are the cleanest human-readable `scope:local` form — the domain is *the authority that allocated the localpart*. XML namespaces ([XML Names](https://www.w3.org/TR/xml-names/)) do the same with prefix→URI indirection: prefixes are document-local conveniences; the URI is the real scope. Both confirm: **surface syntax shows `scope:local`, and the scope component is an authority, not a location.**

## 7. Post-hoc merge — Wikidata, MDM (fetched)

When two Q-ids turn out to be the same, Wikidata *merges* — pooling data into one item and converting the other into a **redirect**, never deleting or reusing the losing id ([Help:Merge](https://www.wikidata.org/wiki/Help:Merge), [Help:Redirects](https://www.wikidata.org/wiki/Help:Redirects)). Identity resolution is layered *on top of* stable identifiers. Answers a question ingest-time design cannot: what happens when sameness is discovered *late*. MDM practice agrees — probabilistic entity resolution stays out of the storage identity layer; the store keeps stable ids plus explicit merge/link records.

---

## Comparison matrix

| Requirement | Best prior art | Mechanism |
|---|---|---|
| 1. Clash-free ingest | RDF merge semantics; Datomic tempids; Neo4j id-spaces | Local ids are scope-qualified; ingest standardizes apart by default |
| 2. Anonymous identity that travels | RDF skolemization (+ its practice gap) | Promote to minted stable id at ingest, deliberately, under a recognizable scheme |
| 3. Deliberate cross-document sameness | Datomic `:db.unique/identity` + lookup refs; *not* raw `owl:sameAs` | Identity-as-key checked at ingest; assertion-based equivalence only if scoped |
| 4. Idempotent re-ingest | Datomic upsert; UUIDv5 determinism | Unique-key upsert; deterministic skolem ids for anonymous elements |
| 5. Frame as namespace | Named graphs; Matrix `@local:server`; Neo4j composite node keys | Container is the scoping unit; `(scope, local)` composite identity |

## The convergent shape

Three independent traditions (RDF, Datomic, Neo4j bulk import) converge on the same architecture:

1. **A document/Frame is a tempid scope.** Bare symbols and `#N` ids mean sameness only within their scope. Ingest = scope-qualified resolution, defaulting to merge (rename-apart). Formalizes the `(frame_id, id)` key RFC-011 already has.
2. **Anonymous ids are skolemized deterministically at ingest** — minted stable ids derived from `(scope-identity × local-name)`, UUIDv5-style, so re-parse yields the same identity. The resolution mapping is returned to the caller, Datomic-style, never hidden.
3. **Cross-document sameness is key-based upsert, not equivalence assertion.** An element that should be "the same" across files carries a unique domain key; ingest resolves it to the existing entity. Assertion-based equivalence, if ever added, must be scoped (the named-graph lesson); post-hoc merge should be Wikidata-style (merge + redirect, never delete).

## Open questions the survey sharpens (the design agenda)

1. **What is the scope's own identity?** File path (location — fragile), content hash (changes when file changes), a declared id in the file header (authority, Matrix/DID-style), or store-assigned at first ingest? Root of the clash-vs-idempotency tension; everything derives from it. The Matrix/DID pattern says: scope should be a declared *authority*, not a derived *location*.
2. **The edited-file problem.** Deterministic skolemization of `#N` is stable across re-*parses* but not re-*orderings* — insert an anonymous element at the top and every ordinal shifts. Prior art has no clean answer (RDF: "don't use blank nodes if you care"). Options: content-derived local names instead of ordinals, or accept anonymous = ephemeral-across-edits and require names for anything durable.
3. **Notation visibility.** Should scoping appear in gram text (Turtle `@prefix` / Matrix `alice@census2020`), or stay purely an ingest-time concern with files staying bare? XML's prefix indirection is the middle path: local convenience syntax, real scope bound in the header.
4. **Post-hoc merge.** Is ingest-time upsert enough, or does the store need a merge-with-redirect operation for sameness discovered late? (May be RFC-010's boundary rather than the new RFC's.)

## Coverage caveats

- The adversarial verify pass concentrated on RDF and Datomic (the two most directly analogous bodies of prior art); those claims are spec-verified against current sources. Neo4j, content-addressing, UUIDv5, Matrix, XML, and Wikidata claims were fetched from primary sources but not formally verified — treat as well-sourced but unaudited.
- The blank-node prevalence statistic is from a 2010 crawl with severe sampling bias; cite qualitatively.
- The `owl:sameAs` "almost always violates" claim is a qualitative assessment from a workshop position paper; later empirical error estimates range ~3–20%.
- Several cross-domain analogies (tempid ≈ document-local id, blank node ≈ `#N`) are this survey's interpretive framings, not claims made by the cited sources.

## Sources

Primary: [RDF 1.1 Concepts](https://www.w3.org/TR/rdf11-concepts/) · [RDF 1.1 Semantics](https://www.w3.org/TR/rdf11-mt/) · [Mallea et al., ISWC 2011 (blank nodes)](https://marceloarenas.cl/publications/iswc11.pdf) · [Halpin/Herman/Hayes, W3C 2010 (sameAs)](https://www.w3.org/2009/12/rdf-ws/papers/ws21) · [Datomic Identity](https://docs.datomic.com/schema/identity.html) · [Datomic transaction data](https://docs.datomic.com/transactions/transaction-data-reference.html) · [Datomic lookup refs](https://blog.datomic.com/2014/02/datomic-lookup-refs.html) · [Neo4j admin import](https://neo4j.com/docs/operations-manual/current/tutorial/neo4j-admin-import/) · [Neo4j elementId/CDC](https://neo4j.com/docs/cdc/current/procedures/elementids-key-properties/) · [Neo4j constraints](https://neo4j.com/docs/cypher-manual/current/constraints/syntax/) · [Perkeep permanodes](https://perkeep.org/doc/schema/permanode) · [RFC 9562 (UUID)](https://datatracker.ietf.org/doc/html/rfc9562) · [IPFS content addressing](https://docs.ipfs.tech/concepts/content-addressing/) · [Unison](https://www.unison-lang.org/docs/the-big-idea/) · [Matrix spec](https://spec.matrix.org/v1.3/appendices/) · [XML Names](https://www.w3.org/TR/xml-names/) · [Wikidata Help:Merge](https://www.wikidata.org/wiki/Help:Merge) · [Wikidata Help:Redirects](https://www.wikidata.org/wiki/Help:Redirects)
