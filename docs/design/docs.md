# Documentation system (Phase 7)

NeoGenera documents itself from three sources that share one document format, and presents them through one
viewer and two exporters.

## Sources

1. **Docstrings.** `defun`, `defmacro`, `defvar`, `defparameter` and `defconstant` accept a docstring. The
   compiler's definition macros (`lisp/compiler/30-driver.lisp`) wrap their expansion with `%with-doc`, which adds a
   `(%put 'name '%doc-function "...")` form. Docstrings therefore live on symbol plists
   (`%doc-function`, `%doc-variable`, `%doc-type`); `documentation` and `(setf documentation)` read and write
   them. System docstrings that predate the macros (the kernel-era library) are attached in bulk by
   `lisp/docs/40-api-docs.lisp` with `%doc-bulk`.
2. **Manuals.** Markdown files in `docs/manual/`, imported at compile time by `import-markdown`, which reads and
   converts the file while the form is being compiled, so the image contains the parsed document and no Markdown
   is read at run time. Editing a manual requires `make world` (the Makefile lists the files as dependencies).
3. **Generated symbol documents.** `symbol-document` builds a document on demand for any symbol: signature (from
   the recorded lambda list), docstring, current value, class structure and the source location recorded by the
   Phase 6 definition notes.

## Document format

A document is a struct (`name title keywords category nodes file`). Nodes are plain s-expressions:

    (:h level inlines...)  (:p inlines...)  (:example "text")  (:ul item...)  (:ol item...)
    (:table row...)        (:see-also topic...)

where an item or cell is a list of inlines, and inlines are strings or
`(:code s) (:em ...) (:strong ...) (:ref topic text) (:fn symbol) (:link url text)`.
The format can be written directly with `define-document` or produced from Markdown. The Markdown subset is
headings, paragraphs, bullet and numbered lists with continuation lines, fenced code blocks, pipe tables, inline
code, emphasis, strong text and links. Link targets `topic:NAME`, `fn:NAME` and `http...` become `:ref`, `:fn` and
`:link`; anything else stays literal text. An unterminated marker such as a lone `*` is literal.

## Search

`doc-index-build` makes one weighted term table per document and per documented symbol (title 5, keyword 3,
heading 3, body 1; symbol name 6, name parts 4, docstring 1). A query is tokenised the same way (lower case,
two characters or more, stop words dropped). An entry scores the capped weight of each query word, with prefix
matches counting half; entries that match every word get a bonus, partial matches are halved. The index is built
on first use and rebuilt when documents are registered. Results are `(score key kind title)`.

## Viewer

`doc-window` is a report window (Phase 6), so every reference is a presentation: `doc-ref` navigates, and
`doc-example` offers *Run in Listener* (default) and *Copy to Listener input*. Back, bookmarks and the contents page
come from the report window machinery. Each page ends with *Candidates*, the related topics: documents first, then
symbols, taken from references on the page, `:see-also` nodes and documents sharing a keyword. Documents open
scrolled to the top (`report-refreshed` hook). Commands: `:doc`, `:search-doc`, `:contents`, `:bookmark`,
`:bookmarks`. The line-mode Listener prints `(doc topic)` as plain text from the same nodes.

## Export

`export-documentation dir` writes `index.html`, one HTML file per document, `reference.html` (every documented
symbol, anchored `sym-NAME`) and `manual.pdf`. Both exporters walk the same nodes. References to unknown topics
degrade to plain text, so a page never links to a missing file. HTML escapes `& < > " '`. The PDF writer is
about 100 lines: Helvetica and Courier, 8-bit text with non-Latin characters replaced, pagination, a correct
cross-reference table; `qpdf --check` and `pdftotext` accept the output.

## Tutorial mode

`deflesson` defines a list of steps (text, an optional `:check` over `form`, `value`, `values`, and a hint). A
Listener window holds `(lesson step failures)` in its `lw-lesson` slot; `%eval-text` calls `tutorial-observe` after each
successful evaluation. A step without a check advances on any successful form. `:hint`, `:skip` and
`:end-tutorial` control the lesson.

## Coverage

`doc-coverage` counts every `COMMON-LISP` symbol that existed when the system was built (snapshotted by
`snapshot-core-symbols` at the end of the manual load, so user and test definitions never count), that names a
function, macro, variable or class, whose definitions are in `lisp/core/` or in the kernel, excluding `%` names and
symbols marked `%internal`. A symbol is covered by a docstring or a mention in a manual. The test suite requires
100%, which makes documentation coverage a CI metric: adding an undocumented public function fails `make test`.

## Deviations from the roadmap

* No separate "reference documentation authored" volume: the API reference is generated from docstrings, and the
  six manuals are hand-written Markdown.
* Examples run in the Listener rather than in a sandbox; they are ordinary forms.
* The GC-stress suite is not run over the documentation tests (the full-heap collection at every safe point makes
  loading the test files take over ten minutes); they are covered by the normal suite and the ASan run.
