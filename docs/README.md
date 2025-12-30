# Pattern Library Documentation

This directory contains user-facing documentation for the Pattern library.

## Structure

```
docs/
├── README.md                    # This file
└── users/
    ├── api/
    │   └── pattern-construction.md  # Pattern construction functions guide
    └── migration/
        └── rename-constructors.md   # Migration guide for constructor renaming
```

## User Documentation

### API Guides

- **[Pattern Construction Functions](users/api/pattern-construction.md)** - Guide to creating atomic patterns using `point`, `pattern`, and `pure`, with porting guidance for other languages

### Migration Guides

- **[Constructor Function Renaming](users/migration/rename-constructors.md)** - Migration guide for the `patternWith` → `pattern` and `pattern` (atomic) → `point` renaming

## Additional Resources

- **Haddock Documentation**: Generate with `cabal haddock` or view online (if published)
- **Design Documentation**: See `design/DESIGN.md` for category-theoretic foundations
- **Feature Specifications**: See `specs/` for detailed feature documentation
- **Examples**: See `libs/pattern/examples/` for usage examples

## For Language Porters

If you're porting the Pattern library to another language, see:

1. **[Pattern Construction Functions](users/api/pattern-construction.md)** - Choose the right function name for your language
2. **[Constitution](../.specify/memory/constitution.md)** - Development principles and multi-language alignment guidelines
3. **Feature Specifications** in `specs/` - Detailed specifications for each feature

The Pattern library is designed as a reference implementation that can be accurately translated to other languages while maintaining category-theoretic correctness.

