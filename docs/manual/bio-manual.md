# Bioinformatics

NeoGenera treats sequences, alignments, features and trees as objects you can click. This manual covers the data types and
file formats, alignment and search, phylogenetics, genomic intervals, calling external tools, the interactive viewers and
four worked workflows. Everything here is in the `COMMON-LISP` package; `(doc 'name)` shows the docstring of any function.

## Sequences

A [bioseq](fn:make-bioseq) holds an identifier, a description, an alphabet (`:dna`, `:rna` or `:protein`, detected from the residues),
upper-case residues and, for reads, Phred qualities. Most functions accept either a `bioseq` or a plain string.

```
(setq s (make-bioseq "ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG" :id "demo"))
(reverse-complement s)       ; => #<DNA demo 39bp CTATCGGGCACCCTTTCAGCGGCC...>
(translate s)                ; => #<PROTEIN demo 13aa MAIVMGR*KGAR*>
(gc-content s)               ; => 0.5897
```

[reverse-complement](fn:reverse-complement), [complement-seq](fn:complement-seq), [transcribe](fn:transcribe), [gc-content](fn:gc-content), [gc-skew](fn:gc-skew) and [gc-windows](fn:gc-windows) cover the basics.
[translate](fn:translate) takes a reading frame and a genetic code table (1 standard, 2 vertebrate mitochondrial, 4, 5, and 11 bacterial);
[kmers](fn:kmers), [kmer-counts](fn:kmer-counts) and [canonical-kmer](fn:canonical-kmer) iterate k-mers; [hamming-distance](fn:hamming-distance) and [edit-distance](fn:edit-distance) compare sequences.
[protein-mass](fn:protein-mass), [hydropathy-profile](fn:hydropathy-profile) and [melting-temperature](fn:melting-temperature) give simple properties.

Large sequences can be stored compactly: [pack-dna](fn:pack-dna) uses 2 bits per base (ambiguous bases are kept as exceptions), [pack-iupac](fn:pack-iupac) 4 bits and
[pack-protein](fn:pack-protein) 5 bits. [packed-ref](fn:packed-ref), [packed-subseq](fn:packed-subseq) and [unpack-seq](fn:unpack-seq) read them back.

[find-orfs](fn:find-orfs) scans three frames on one or both strands and returns `orf` objects with zero-based forward-strand coordinates, the strand and the protein.

## File formats

| Format | Read | Result |
|---|---|---|
| FASTA | [read-fasta](fn:read-fasta), [map-fasta](fn:map-fasta) (streaming), [write-fasta](fn:write-fasta) | `bioseq` list |
| FASTQ | [read-fastq](fn:read-fastq) | reads with a Phred vector |
| GenBank | [read-genbank](fn:read-genbank) | records with features and sequence |
| GFF3 and GTF | [read-gff](fn:read-gff) | `bio-feature` list |
| BED | [read-bed](fn:read-bed) | `bio-feature` list |
| VCF | [read-vcf](fn:read-vcf) | `vcf-record` list and sample names |
| SAM | [read-sam](fn:read-sam) | `sam-record` list and header |
| BAM | [read-bam](fn:read-bam) | the same records, read-only |

Paths ending in `.gz` or `.bgz` are decompressed with the system `gzip`. Every `parse-...` function takes a list of lines instead of a path.
Coordinates in the objects are always zero-based and half-open, whatever the file used. [feature-sequence](fn:feature-sequence) extracts the sequence of a feature
from a genome (joining intervals and reverse-complementing the minus strand); [vcf-genotype](fn:vcf-genotype) decodes a genotype; [cigar-string](fn:cigar-string),
[sam-end](fn:sam-end) and [pileup-depth](fn:pileup-depth) work with alignments.

## Alignment

[align](fn:align) does Needleman-Wunsch (`:global`), Smith-Waterman (`:local`) or semi-global alignment with affine gap penalties; a gap of length k costs
`gap-open + (k-1) * gap-extend`. The fill is a C kernel; pass `:band n` to restrict it to a diagonal band for long, similar sequences.
Protein alignments use BLOSUM62 by default (`*blosum62*`, `*pam250*` and [nucleotide-matrix](fn:nucleotide-matrix) are provided; [make-scoring-matrix](fn:make-scoring-matrix) builds others).

```
(align "TGTTACGG" "GGTTGACTA" :mode :local :matrix (nucleotide-matrix 3 -3) :gap-open 2 :gap-extend 2)
(princ (alignment-string (align "HEAGAWGHEE" "PAWHEAE" :mode :local) :matrix *blosum62*))
```

An `alignment` has `alignment-score`, the two gapped strings, coordinates, [alignment-identity](fn:alignment-identity) and [alignment-midline](fn:alignment-midline).

## Search and motifs

[build-suffix-index](fn:build-suffix-index) builds a suffix array in C; [suffix-index-search](fn:suffix-index-search) finds every occurrence of a pattern and [longest-repeat](fn:longest-repeat) the longest repeated substring.
[build-kmer-index](fn:build-kmer-index) and [seeded-search](fn:seeded-search) give a BLAST-like search: exact seeds are grouped by diagonal and extended with banded local alignment.
[find-iupac-motif](fn:find-iupac-motif) matches ambiguity codes, [make-pwm](fn:make-pwm) builds a position weight matrix from aligned sites and [pwm-scan](fn:pwm-scan) scores windows on one or both strands.
[regex-find-all](fn:regex-find-all) is a small regular-expression engine (literals, `.`, sets, groups, `|`, `* + ? {m,n}`, anchors) for sequence patterns such as `TTGACA[ACGT]{15,19}TATAAT`.

## Phylogenetics

[distance-matrix](fn:distance-matrix) computes p, Jukes-Cantor or Kimura distances between sequences (aligning first when lengths differ). [upgma](fn:upgma) and [neighbor-joining](fn:neighbor-joining)
build trees; [tree-from-sequences](fn:tree-from-sequences) does both steps. Trees are `phylo-node` objects: [tree-newick](fn:tree-newick) and [parse-newick](fn:parse-newick) convert to and from Newick,
[tree-ascii](fn:tree-ascii) draws one, [tree-patristic-distances](fn:tree-patristic-distances) gives path lengths and [robinson-foulds](fn:robinson-foulds) compares topologies.

## Genomic intervals

[make-interval-tree](fn:make-interval-tree) answers overlap queries in O(log n + k); [build-feature-index](fn:build-feature-index) indexes features by sequence. [merge-intervals](fn:merge-intervals),
[interval-complement](fn:interval-complement), [intersect-intervals](fn:intersect-intervals), [coverage-depth](fn:coverage-depth) and [coverage-segments](fn:coverage-segments) cover the usual arithmetic.

## External tools

[run-tool](fn:run-tool) starts a program with an argument list; no shell is involved, program names are validated and a missing program or non-zero exit is an error unless
`:check nil`. Output becomes objects: [run-blast](fn:run-blast) and [parse-blast-tabular](fn:parse-blast-tabular) give `blast-hit` structures, [run-minimap2](fn:run-minimap2) and [parse-paf](fn:parse-paf) give `paf-record` structures,
[samtools-depth](fn:samtools-depth) and [samtools-view](fn:samtools-view) wrap samtools.

## Interactive viewers

The value of any form that returns a sequence, alignment, tree, feature, ORF or residue is a presentation. [show-sequence](fn:show-sequence) opens a sequence in a window with
every residue clickable (describe it, translate from there, reverse-complement the next 30 bases); [show-alignment](fn:show-alignment) shows an alignment with clickable columns;
[show-tree](fn:show-tree) shows a tree where clicking a clade zooms to it and the menu offers Newick, leaves and height; [show-features](fn:show-features) lists features.
`:bio-view expr` opens whatever an expression returns in the right viewer, and [bio-view](fn:bio-view) does the same from Lisp.

## Worked workflows

**Annotate a genome.** [annotate-orfs](fn:annotate-orfs) turns every ORF of a minimum length into a CDS feature with a translation; [genome-report](fn:genome-report) summarises length, GC and coding fraction.

```
(setq g (first (read-fasta "genome.fa.gz")))
(setq cds (annotate-orfs g :min-length 75))
(genome-report g)
(show-features cds)
```

**Search a protein against a proteome.** [rank-by-alignment](fn:rank-by-alignment) aligns a query against every protein of [cds-proteins](fn:cds-proteins) and returns subject, alignment and bit score, best first.

**Summarise variants.** [variant-summary](fn:variant-summary) counts SNPs, insertions and deletions, the transition/transversion ratio and variants per chromosome from VCF records.

**Check read quality.** [fastq-qc](fn:fastq-qc) reports read counts, quality, GC, N content, duplicates and per-position quality; [coverage-summary](fn:coverage-summary) summarises a depth vector.

**Design primers.** [design-primers](fn:design-primers) proposes PCR primer pairs flanking a target region, filtering on length, GC, melting temperature, a G/C clamp, homopolymer runs and self-complementarity.

**Enrichment.** [enrichment](fn:enrichment) tests annotation categories for over-representation with one-sided Fisher tests and Benjamini-Hochberg correction, using the statistics library of the
[Mathematics and Statistics](topic:math-manual) manual.

See also [The Documentation System](topic:documentation-system) for how this manual is built.
