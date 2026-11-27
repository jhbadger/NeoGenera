# Security review (1.0)

NeoGenera is a programming environment: a Lisp system can read, write and delete files, run programs and evaluate
code, because that is what a Lisp system is for. The security goal is therefore not a sandbox but this narrower set:

1. Data that is not code (data files, parsed formats, settings, program output) must never be executed.
2. Hostile or corrupt input must produce an ordinary, recoverable Lisp error, never memory corruption or a crash.
3. The facilities that touch the outside world (files, subprocesses, images) must have no avoidable sharp edges.
4. Code you load or evaluate, and heap images you start from, are programs and are trusted like any program.

## Facilities reviewed

| Facility | Finding | State |
|---|---|---|
| Subprocesses (`run-program`, `run-tool`) | Started with `execvp` and an argument list; there is no code path from a string to a shell. `run-tool` additionally validates program names, requires string arguments, and reports missing programs and non-zero exits. | Reviewed, no change needed |
| Subprocess I/O | The old implementation wrote all of stdin before reading any output, so a child that produces output before consuming input could deadlock the system, and a child that closed stdin early raised SIGPIPE. | **Fixed**: `poll`-driven simultaneous feed and drain, SIGPIPE ignored during the call; tested with 2.5 MB each way |
| `gzip` helper | The path was passed as an option position, so a file named `-x` would be read as an option. | **Fixed**: `gzip -dc -- path` |
| Files | `open`, `delete-file`, rename and mkdir run with the user's privileges and have no sandbox. | By design; documented |
| Network | The kernel has no socket, `dlopen`, `system` or `popen` calls. | None to review |
| Reader | `#.` evaluates code and is controlled by `*read-eval*`; the reader checks it. Loaders for data (settings, parsers) must bind it to NIL, and the settings loader does. | Reviewed |
| Format strings in C | Every `lisp_error` call in the kernel passes a literal format; `type_error` formats the offending value with `%s`. | Reviewed, no change needed |
| Heap images | An image is a raw dump of the heap, so loading one is running its code. A checksum now detects accidental corruption and truncation, headers are range-checked, and the checksum is verified *before* the live heap is touched. Images are not authenticated: do not load images from untrusted sources. | **Hardened** (format NGIMG004) |
| Binary fasl files | A fasl file is compiled code, so loading one is running it and is as trusted as loading source; fasl files are not authenticated. The decoder (`src/fasl.c`) still treats the bytes as untrusted as far as memory safety goes: magic, version, CRC-32 and the entry table are checked before decoding; every length is bounded by the bytes that remain and nesting by the C stack guard; each entry must be consumed exactly; and every instruction stream is verified (known opcodes and operand counts, constant indices, jump targets on instruction boundaries, no running off the end, `CLOSURE` constants that are code, `CHKRANGE` constants that are fixnum pairs). Stack depth and local slot numbers are *not* verified. | **New**, fuzzed (`binary-fasl-fuzz`, `binary-fasl-validates-code`) |
| Declared types at `safety 0` | Code compiled with `(optimize (safety 0))` trusts its type declarations and uses unchecked instructions on them; a false declaration (a non-flonum declared `double-float`, a non-simple vector declared `simple-vector`) can crash the process. Untyped code is never given unchecked instructions, and the default policy (`safety 1`) checks every declaration it uses. | By design; documented in docs/design/compiler.md |
| Image saving | Images were written in place, so a crash or full disk destroyed the previous image. | **Fixed**: write to a temporary file, `fsync`, then rename |
| Allocation | A single huge request (for example `(make-string 4000000000)`) aborted the process, and the size arithmetic could overflow. | **Fixed**: requests are compared against the free space before any pointer arithmetic and fail with a Lisp error; a 4 MB reserve lets error handling run when the heap is nearly full |
| Stack depth | Runaway recursion crashed the process with SIGSEGV: the evaluator's protected-object stack was a fixed array with no bounds check, VM frame exhaustion could not be handled because handling needed frames, and C recursion (reader, printer, `equal`) had no stack guard. | **Fixed**: bounds-checked root stack, error-handling reserves for the root stack and the VM frame and value stacks, and a C stack-address guard; runaway recursion is now `Control stack exhausted`, repeatedly recoverable |
| BAM parser | Found by fuzzing: a corrupt length field could request gigabytes. | **Fixed**: every count and length is checked against the data that remains |
| Temporary files | The library never creates predictable temporary files; test and example scripts use fixed names in `/tmp` and are not meant to be run on shared machines. | Documented |

The bytecode assembler (`lisp/asm/`, `defasm`) is the one place a user supplies raw instructions. Nothing it builds reaches the VM unverified:
every routine is checked by abstract interpretation for stack depth (no underflow, equal depth where paths meet), frame slots, jump
targets, dynamic-binding balance and falling off the end, and the instructions that trust their operands' types (`addfx`, `carx`,
`unbox`, `addfl`, ...) are accepted only on operands the verifier has proved to have that type; `svrefx`/`svsetx` need a routine
declared `(:unsafe)`, which waives the type proofs (like `(safety 0)`) but never the stack and control-flow checks. `%make-code` itself
does no checking and is not part of the interface. Fuzzed (`fuzz-asm`: random instruction soups must assemble or signal `asm-error`, and
anything that assembles must run without crashing the process).

## Fuzzing

`tests/lisp/fuzz-tests.lisp` (part of `make test`, and run under ASan and UBSan by `make fuzz` and in CI) feeds random and mutated
input to the reader, `format`, every file-format parser (FASTA, FASTQ, GenBank, GFF, BED, VCF, SAM, BAM, Newick, CSV, Markdown),
the binary fasl decoder (mutated payloads, which must decode or signal an error and are never run), the regular-expression engine, the alignment and suffix-array kernels with arbitrary strings and gap parameters, bignum identities across
the Karatsuba boundary, and floating-point edge values (infinities and NaN). A function may succeed or signal an error; it may not
crash. The fuzzers found two real defects (the BAM allocation above and, with the stack tests, the recursion crash).

## Reporting a vulnerability

Please report suspected vulnerabilities privately to the maintainers (see the repository's security contact) with a reproducer;
do not open a public issue. We aim to acknowledge reports within a week.
