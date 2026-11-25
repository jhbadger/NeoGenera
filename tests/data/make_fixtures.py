#!/usr/bin/env python3
"""Regenerate the small bioinformatics test fixtures (tests/data/tiny.*).  Standard library only."""
import gzip, struct, os
here = os.path.dirname(os.path.abspath(__file__))
def w(name, text, mode="w"):
    with open(os.path.join(here, name), mode) as f: f.write(text)

w("tiny.fa", """>seq1 first test sequence
ACGTACGTACGTACGTACGTACGTACGTACGTAC
GTACGTAC
>seq2
ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG
; a comment
>seq3 protein
MKVLAAGIVGLLLAQPSWA
""")
w("tiny.fq", """@read1 sample A
ACGTNACGTA
+
IIIIIIII#5
@read2
GGGGCCCCAA
+
5555IIIIII
""")
w("tiny.gb", """LOCUS       TESTSEQ                   60 bp    DNA     linear   BCT 01-JAN-2020
DEFINITION  Test sequence with two genes.
ACCESSION   TS000001
SOURCE      Testus organismus
  ORGANISM  Testus organismus
            Bacteria.
FEATURES             Location/Qualifiers
     source          1..60
                     /organism="Testus organismus"
     gene            1..18
                     /gene="geneA"
     CDS             1..18
                     /gene="geneA"
                     /product="hypothetical
                     protein A"
                     /translation="MKVLAA"
     CDS             complement(join(25..36,40..51))
                     /gene="geneB"
ORIGIN
        1 atgaaagtac tggcggcgta aggcatcgat tggatcctga cgttagcgat ctagctagca
//
""")
w("tiny.gff3", """##gff-version 3
chr1	test	gene	101	200	.	+	.	ID=g1;Name=alpha
chr1	test	CDS	101	196	.	+	0	ID=c1;Parent=g1
chr1	test	gene	301	500	.	-	.	ID=g2;Name=beta
chr2	test	gene	11	60	5.5	+	.	ID=g3
""")
w("tiny.gtf", """chr1	src	exon	101	200	.	+	.	gene_id "G1"; transcript_id "T1";
chr1	src	exon	301	400	.	+	.	gene_id "G1"; transcript_id "T1";
""")
w("tiny.bed", """track name=tiny
chr1	100	200	featA	500	+
chr1	150	250	featB	300	-
chr2	0	50	featC
""")
w("tiny.vcf", """##fileformat=VCFv4.2
#CHROM	POS	ID	REF	ALT	QUAL	FILTER	INFO	FORMAT	S1	S2
chr1	100	rs1	A	G	50	PASS	DP=30;AF=0.5	GT:DP	0/1:15	1/1:15
chr1	200	.	C	T	40	PASS	DP=20	GT:DP	0/0:10	0|1:10
chr1	300	.	G	GA	30	q10	DP=10;DB	GT:DP	0/1:5	./.:0
chr2	50	.	TAC	T	60	PASS	DP=40	GT:DP	1/1:20	0/1:20
chr2	60	.	A	C	20	PASS	.	GT	0/1	1/1
""")
sam = [
 "@HD\tVN:1.6\tSO:coordinate",
 "@SQ\tSN:chr1\tLN:100",
 "@SQ\tSN:chr2\tLN:50",
 "r1\t0\tchr1\t5\t60\t8M\t*\t0\t0\tACGTACGT\tIIIIIIII\tNM:i:0\tRG:Z:grp1",
 "r2\t16\tchr1\t10\t30\t4M2I4M\t*\t0\t0\tACGTTTACGT\t5555555555\tNM:i:2",
 "r3\t0\tchr2\t3\t60\t5M2D5M\t*\t0\t0\tGGGGGCCCCC\tIIIIIIIIII\tNM:i:2",
 "r4\t4\t*\t0\t0\t*\t*\t0\t0\tACGN\tII#I\t",
]
w("tiny.sam", "\n".join(l.rstrip("\t") for l in sam) + "\n")

# a BAM equivalent of the SAM above (BGZF-compatible gzip; the reader decompresses with gzip -dc)
refs = [("chr1", 100), ("chr2", 50)]
ref_index = {n: i for i, (n, _) in enumerate(refs)}
header_text = "\n".join(l for l in sam if l.startswith("@")) + "\n"
out = bytearray(b"BAM\x01")
out += struct.pack("<i", len(header_text)) + header_text.encode()
out += struct.pack("<i", len(refs))
for n, l in refs: out += struct.pack("<i", len(n) + 1) + n.encode() + b"\0" + struct.pack("<i", l)
OPS = "MIDNSHP=X"; NIB = "=ACMGRSVTWYHKDBN"
def cigar_ops(text):
    import re
    return [(int(n), OPS.index(o)) for n, o in re.findall(r"(\d+)([MIDNSHP=X])", text)] if text != "*" else []
for line in sam:
    if line.startswith("@"): continue
    f = line.split("\t")
    qname, flag, rname, pos, mapq, cig = f[0], int(f[1]), f[2], int(f[3]), int(f[4]), f[5]
    seq, qual = f[9], f[10]
    tags = f[11:]
    rid = ref_index.get(rname, -1)
    ops = cigar_ops(cig)
    rec = struct.pack("<iiBBHHHiiii", rid, pos - 1, len(qname) + 1, mapq, 0, len(ops), flag, len(seq), -1, -1, 0)
    rec += qname.encode() + b"\0"
    for n, o in ops: rec += struct.pack("<I", (n << 4) | o)
    packed = bytearray()
    for i in range(0, len(seq), 2):
        hi = NIB.index(seq[i]); lo = NIB.index(seq[i + 1]) if i + 1 < len(seq) else 0
        packed.append((hi << 4) | lo)
    rec += bytes(packed)
    rec += bytes(ord(c) - 33 for c in qual)
    for t in tags:
        if not t: continue
        name, typ, val = t.split(":", 2)
        if typ == "i": rec += name.encode() + b"C" + struct.pack("<B", int(val))
        elif typ == "Z": rec += name.encode() + b"Z" + val.encode() + b"\0"
    out += struct.pack("<i", len(rec)) + rec
with gzip.open(os.path.join(here, "tiny.bam"), "wb") as g: g.write(bytes(out))
with gzip.open(os.path.join(here, "tiny.fa.gz"), "wt") as g: g.write(open(os.path.join(here, "tiny.fa")).read())
print("fixtures written to", here)
