# The Documentation System

NeoGenera documents itself from three sources that share one document format: manuals written in Markdown,
docstrings in the code, and generated pages for every symbol. The viewer, the HTML export and the PDF export all
render the same nodes.

## Reading

| Command | What it shows |
|---|---|
| `:doc` | the table of contents |
| `:doc user-guide` | a manual topic |
| `:doc mapcar` | a symbol: signature, docstring, value, class structure and where it is defined |
| `:search-doc words` | ranked results across manuals and docstrings |
| `:bookmark`, `:bookmarks` | save and list pages |
| `:tutorial` | interactive lessons |

In a documentation window click a link to follow it and press Backspace to go back; `b` bookmarks the page and
`t` shows the contents. Every page ends with **Candidates**, the topics most related to it. Click an example
and choose *Run in Listener* to try it. A name in code font that is a known function is a link as well.

## Writing docstrings

A string after the lambda list of [defun](fn:defun) or [defmacro](fn:defmacro), and after the value in
[defvar](fn:defvar) and [defparameter](fn:defparameter), is recorded. Read it with
[documentation](fn:documentation); change it with `(setf (documentation 'name 'function) "...")`.
Docstrings may use the Markdown inline syntax: `code`, *emphasis*, **strong**.

## Writing manuals

A manual is a Markdown file. [import-markdown](fn:import-markdown) reads it when the system is built, so
its text becomes part of the image:

```
(import-markdown :editor-manual "docs/manual/editor-manual.md" :category :manual :keywords (editor emacs))
```

Supported Markdown: headings, paragraphs, fenced code blocks (examples), bulleted and numbered lists, tables,
`code`, *emphasis*, **strong** and links. Link targets are `topic:name` (another document), `fn:symbol`
(a symbol's documentation), or a web address. Documents can also be written directly as s-expressions with
[define-document](fn:define-document): nodes such as `(:h 2 "Title")`, `(:p "text" (:code "x"))`,
`(:example "(+ 1 2)")`, `(:ul ...)` and `(:see-also :topic)`.

## Exporting

`:export-docs directory` (or [export-documentation](fn:export-documentation)) writes `index.html`, one HTML
file per document, `reference.html` (every documented symbol with its lambda list) and `manual.pdf`.

## Coverage

[doc-coverage](fn:doc-coverage) reports how many exported symbols of the core library have a docstring or are
mentioned in a manual, and lists the ones that do not. The test suite fails if coverage drops below its floor.

## Tutorials

[deflesson](fn:deflesson) defines a lesson as a list of steps, each with text, an optional check and a hint.
`:tutorial` starts a lesson in the Listener; the Listener checks each form you evaluate, and `:hint`, `:skip` and
`:end-tutorial` help along the way.
