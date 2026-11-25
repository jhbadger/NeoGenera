## What this changes

## Checklist
- [ ] `make test` passes (C, Lisp, pty); `make world` still reaches the fixed point
- [ ] New public functions have docstrings (the documentation-coverage test requires it)
- [ ] New behaviour has tests; parsers and kernels also have fuzz cases
- [ ] If C code changed: `make fuzz` (ASan/UBSan) is clean and docs/design describes any new invariant
- [ ] CHANGELOG.md updated for user-visible changes
