# Feature Specification: Documentation Reorganization

**Feature Branch**: `027-docs-reorganization`  
**Created**: 2025-01-28  
**Status**: Draft  
**Input**: User description: "Reorganize the documentation again, to simplify and remove redundancy as discussed above"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Find User Documentation Easily (Priority: P1) 🎯 MVP

As a developer using the pattern-hs library, I need to find user documentation quickly without navigating through multiple nested directories, so that I can start learning and using the library efficiently.

**Why this priority**: This is the primary use case - developers need clear, accessible documentation. The current structure with `docs/users/guide/` is unnecessarily nested, and having both `docs/users/guide/` and `docs/users/guides/` creates confusion. Moving the excellent numbered guide to `docs/guide/` makes it immediately discoverable.

**Independent Test**: Can be fully tested by verifying that a new user can find the user guide at `docs/guide/01-introduction.md` without navigating through `docs/users/guide/`, and that all guide files are accessible at the new location. This delivers immediate clarity and reduces navigation friction.

**Acceptance Scenarios**:

1. **Given** a developer wants to learn about Patterns, **When** they look in `docs/guide/`, **Then** they find the numbered guide files (01-introduction.md through 08-gram-notation-reference.md)
2. **Given** a developer follows a link to user documentation, **When** they access `docs/guide/01-introduction.md`, **Then** they see the introduction content without broken links
3. **Given** a developer reads guide section 03-construction.md, **When** they follow internal links to other guide sections, **Then** all links resolve correctly to the new `docs/guide/` location
4. **Given** a developer accesses the root README.md, **When** they follow the documentation link, **Then** it points to `docs/guide/01-introduction.md` instead of `docs/users/README.md`

---

### User Story 2 - Find Reference Documentation Without Redundancy (Priority: P1)

As a developer porting the library to another language, I need to find all reference documentation (porting guides, migration guides, API porting guidance) in a single location without duplicate or conflicting information, so that I can efficiently port the library while maintaining correctness.

**Why this priority**: Porters need authoritative reference material. Currently, migration guides and API porting guidance are in `docs/users/` which is confusing since they're not user-facing. Moving them to `docs/reference/` consolidates all porter-focused documentation and eliminates the false impression that they're user guides.

**Independent Test**: Can be fully tested by verifying that all reference material (migration guides, API porting guidance) is accessible in `docs/reference/` and that links from porting guide and architecture docs resolve correctly. This delivers consolidated reference documentation for porters.

**Acceptance Scenarios**:

1. **Given** a porter needs migration guidance, **When** they look in `docs/reference/migration/`, **Then** they find rename-constructors.md
2. **Given** a porter needs API porting guidance, **When** they look in `docs/reference/features/`, **Then** they find pattern-construction.md with porting guidance
3. **Given** a porter reads PORTING-GUIDE.md, **When** it references migration or API documentation, **Then** links resolve to `docs/reference/` locations
4. **Given** a porter accesses reference documentation, **When** they navigate the structure, **Then** they don't encounter duplicate or conflicting information from deleted `docs/users/guides/` files

---

### User Story 3 - Navigate Documentation Without Redundant README Files (Priority: P2)

As a developer browsing the documentation, I need a clean directory structure without redundant README files that duplicate navigation information, so that I can focus on the actual documentation content rather than navigating through multiple index files.

**Why this priority**: Redundant README files create maintenance overhead and confusion. The numbered guide files are self-explanatory, and the directory structure is clear enough without nested README files. Removing them simplifies navigation and reduces maintenance burden. However, this is less critical than the core reorganization, so it's P2.

**Independent Test**: Can be fully tested by verifying that all README.md files in `docs/users/`, `docs/users/guide/`, `docs/reference/`, and `docs/design/` are removed, and that navigation still works through direct file access and the main `docs/README.md`. This delivers cleaner structure and reduced maintenance.

**Acceptance Scenarios**:

1. **Given** a developer navigates to `docs/guide/`, **When** they list files, **Then** they see only numbered guide files (no README.md)
2. **Given** a developer navigates to `docs/reference/`, **When** they list files, **Then** they see reference documents without a README.md file
3. **Given** a developer accesses documentation, **When** they follow links, **Then** they don't encounter broken links that previously pointed to deleted README files
4. **Given** a developer needs an overview, **When** they access `docs/README.md`, **Then** it provides navigation without relying on nested README files

---

### Edge Cases

- What happens when external links (from other repos, websites, or bookmarks) point to old `docs/users/guide/` paths? → These will break and need to be updated, but this is acceptable for a reorganization that improves the structure
- How do we handle references in historical spec files that mention `docs/users/guide/`? → These are historical artifacts and don't need updating, but we should note the change
- What if someone has bookmarked specific guide sections? → Bookmarks will break, but the new structure is clearer and worth the temporary inconvenience
- How do we ensure no broken internal links after moving files? → All guide files need link updates, and we should verify all cross-references

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST move `docs/users/guide/` directory and all contents to `docs/guide/`
- **FR-002**: System MUST move `docs/users/migration/rename-constructors.md` to `docs/reference/migration/rename-constructors.md`
- **FR-003**: System MUST move `docs/users/api/pattern-construction.md` to `docs/reference/features/pattern-construction.md`
- **FR-004**: System MUST delete `docs/users/guides/` directory and all contents (getting-started.md, pattern-construction.md, gram-serialization.md, graph-lens.md)
- **FR-005**: System MUST delete `docs/users/examples/` directory and all contents (basic-patterns.md, graph-operations.md)
- **FR-006**: System MUST delete `docs/users/api/` directory after moving pattern-construction.md
- **FR-007**: System MUST delete `docs/users/migration/` directory after moving rename-constructors.md
- **FR-008**: System MUST delete `docs/users/` directory after all moves and deletions are complete
- **FR-009**: System MUST delete `docs/users/guide/README.md` file (no longer needed)
- **FR-010**: System MUST delete `docs/users/README.md` file (no longer needed)
- **FR-011**: System MUST delete `docs/reference/README.md` file (no longer needed)
- **FR-012**: System MUST delete `docs/design/README.md` file (no longer needed)
- **FR-013**: System MUST update `README.md` (root) to point to `docs/guide/01-introduction.md` instead of `docs/users/README.md`
- **FR-014**: System MUST update all internal links in guide files (01-introduction.md through 08-gram-notation-reference.md) to reflect new paths
- **FR-015**: System MUST update `docs/reference/PORTING-GUIDE.md` to update "For Library Users" link if it references `docs/users/README.md`
- **FR-016**: System MUST update `docs/reference/ARCHITECTURE.md` to update any links that reference `docs/users/` paths
- **FR-017**: System MUST update `docs/reference/migration/rename-constructors.md` to update any links that reference moved files
- **FR-018**: System MUST update `docs/reference/features/pattern-construction.md` to update any links that reference moved files
- **FR-019**: System MUST ensure all guide files maintain correct relative links between numbered sections
- **FR-020**: System MUST verify no broken links exist after reorganization

### Key Entities

- **User Guide**: The numbered guide files (01-introduction.md through 08-gram-notation-reference.md) that provide comprehensive user documentation. Located at `docs/guide/` after reorganization.
- **Reference Documentation**: Porter-focused documentation including porting guides, migration guides, API porting guidance, architecture docs, and feature specifications. Located at `docs/reference/` after reorganization.
- **Migration Guide**: Documentation for migrating between library versions (rename-constructors.md). Moved from `docs/users/migration/` to `docs/reference/migration/`.
- **API Porting Guide**: Documentation for porting API functions to other languages (pattern-construction.md). Moved from `docs/users/api/` to `docs/reference/features/`.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All guide files are accessible at `docs/guide/` with 100% of files successfully moved and accessible
- **SC-002**: All reference documentation is consolidated in `docs/reference/` with 100% of migration and API porting guides moved to appropriate subdirectories
- **SC-003**: Zero broken internal links exist after reorganization, verified by checking all markdown files for relative links
- **SC-004**: Root README.md correctly points to new guide location, with link verification showing correct path
- **SC-005**: All redundant directories (`docs/users/guides/`, `docs/users/examples/`, `docs/users/api/`, `docs/users/migration/`, `docs/users/`) are removed with 100% deletion verification
- **SC-006**: All redundant README.md files are removed with verification that no README.md exists in `docs/users/`, `docs/users/guide/`, `docs/reference/`, or `docs/design/` (except main `docs/README.md`)
- **SC-007**: Documentation structure is simplified with user guide at top level (`docs/guide/`) and all reference material consolidated (`docs/reference/`), verified by directory structure review

## Assumptions

- The numbered guide files (01-08) are the authoritative user documentation and should be preserved
- The `docs/users/guides/` files are inferior duplicates that can be safely deleted
- The `docs/users/examples/` files are redundant with examples already in the guide
- Migration guides are reference material for porters, not user-facing documentation
- API porting guidance is reference material for porters, not user-facing documentation
- README files in subdirectories are redundant when directory structure is self-explanatory
- The main `docs/README.md` should be kept as the top-level documentation index
- Historical spec files that reference old paths don't need updating (they're historical artifacts)
- External links may break but this is acceptable for improved structure
- All internal cross-references between guide sections can be updated to use relative paths

## Dependencies

- **Prerequisites**: Existing documentation structure from feature 026-pattern-documentation (✅ Complete)
- **Prerequisites**: Existing reference documentation structure from feature 022-documentation-reorganization (✅ Complete)
- **No blocking dependencies**: This is a reorganization task that can proceed independently

## Notes

- This reorganization simplifies the documentation structure by:
  - Moving user guide to top level (`docs/guide/`) for immediate discoverability
  - Consolidating all reference material in `docs/reference/` for porters
  - Removing redundant directories and files
  - Eliminating unnecessary README files that duplicate navigation
- The reorganization preserves all valuable content while improving structure
- Some external links may break, but the improved structure justifies this temporary inconvenience
- Historical spec files may reference old paths, but these are historical artifacts and don't need updating
