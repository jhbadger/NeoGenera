#!/usr/bin/env python3
"""Record a scripted tour of the window system as an asciicast (docs/tour.cast), playable with
`asciinema play docs/tour.cast` or the asciinema web player.  Standard library only.

The recording is made by driving the real program through a pseudo-terminal, so what you see is what the
program printed; nothing is mocked.  Re-run it after UI changes:  python3 packaging/record-tour.py
"""
import json, os, pty, select, struct, sys, time, fcntl, termios

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "docs", "tour.cast")
COLS, ROWS = 100, 30

STEPS = [  # (seconds to wait first, text to type, then Return?)
    (1.2, "(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))", True),
    (0.8, "(fact 30)", True),
    (0.8, "(/ 1 3)", True),
    (0.8, "(rng-seed 3)", True),
    (0.8, '(princ (plot-histogram (loop repeat 400 collect (rand-normal)) :bins 10 :width 30 :title "400 normal draws"))', True),
    (1.5, "(translate \"ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG\")", True),
    (1.5, "(princ (alignment-string (align \"HEAGAWGHEE\" \"PAWHEAE\" :mode :local) :matrix *blosum62*))", True),
    (1.8, "(differentiate '(* (sin x) (exp x)) 'x)", True),
    (1.5, ":search-doc alignment matrix", True),
    (2.5, "", False),
]

def main():
    pid, fd = pty.fork()
    if pid == 0:
        os.environ.update(TERM="xterm-256color", COLUMNS=str(COLS), LINES=str(ROWS), NG_SOURCE_ROOT=ROOT)
        os.execv(os.path.join(ROOT, "bin", "neogenera"), ["neogenera", "--ui"])
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", ROWS, COLS, 0, 0))
    events, t0 = [], time.time()

    def pump(duration):
        end = time.time() + duration
        while time.time() < end:
            r, _, _ = select.select([fd], [], [], 0.05)
            if r:
                try: data = os.read(fd, 65536)
                except OSError: return False
                if not data: return False
                events.append([round(time.time() - t0, 3), "o", data.decode("utf-8", "replace")])
        return True

    pump(1.0)
    for wait, text, ret in STEPS:
        pump(wait)
        for ch in text:                     # type like a person
            os.write(fd, ch.encode()); pump(0.02)
        if ret: os.write(fd, b"\r")
    pump(1.0)
    os.write(fd, b"\x1b[21~")               # F10 quits
    pump(1.0)
    try: os.waitpid(pid, os.WNOHANG)
    except OSError: pass
    header = {"version": 2, "width": COLS, "height": ROWS, "timestamp": int(t0), "title": "NeoGenera tour",
              "env": {"TERM": "xterm-256color", "SHELL": "/bin/sh"}}
    with open(OUT, "w") as f:
        f.write(json.dumps(header) + "\n")
        for e in events: f.write(json.dumps(e) + "\n")
    print("wrote", OUT, len(events), "events,", round(events[-1][0], 1), "seconds")

if __name__ == "__main__":
    main()
