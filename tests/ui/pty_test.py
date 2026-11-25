#!/usr/bin/env python3
"""pty_test.py -- run `bin/neogenera --ui` in a pseudo-terminal and check what a user would see.

A small VT100/xterm emulator replays the exact bytes the program writes (cursor addressing,
clear, UTF-8 text; colours are ignored), so this validates the screen diffing, the input parser
(keys, mouse, resize) and terminal setup/restore against a real tty.
"""
import os, pty, sys, time, select, struct, fcntl, termios, signal, re

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

class Term:
    def __init__(self, rows, cols):
        self.resize(rows, cols); self.buf = b""; self.cursor_visible = None
        self.alt_screen = False; self.mouse = False; self.raw = b""
    def resize(self, rows, cols):
        self.rows, self.cols = rows, cols
        self.grid = [[" "] * cols for _ in range(rows)]; self.r = self.c = 0
    def feed(self, data):
        self.raw += data; self.buf += data
        text = self.buf.decode("utf-8", errors="ignore") if False else None
        # decode what is complete; keep a trailing partial sequence for next time
        try:
            s = self.buf.decode("utf-8"); self.buf = b""
        except UnicodeDecodeError as e:
            s = self.buf[:e.start].decode("utf-8", errors="replace"); self.buf = self.buf[e.start:]
        i = 0
        while i < len(s):
            ch = s[i]
            if ch == "\x1b":
                m = re.compile(r"\x1b\[([?0-9;]*)([A-Za-z])").match(s, i)
                if not m:
                    if len(s) - i < 24 and "[" in s[i:i+2] + "[": self.buf = s[i:].encode("utf-8"); return
                    i += 1; continue
                params, final = m.group(1), m.group(2); i = m.end()
                if final == "H":
                    p = [int(x) if x else 1 for x in params.split(";")] if params else [1, 1]
                    self.r = min(self.rows - 1, p[0] - 1); self.c = min(self.cols - 1, (p[1] if len(p) > 1 else 1) - 1)
                elif final == "J":
                    self.grid = [[" "] * self.cols for _ in range(self.rows)]
                elif final in "hl" and params.startswith("?"):
                    on = final == "h"
                    for code in params[1:].split(";"):
                        if code == "25": self.cursor_visible = on
                        elif code == "1049": self.alt_screen = on
                        elif code in ("1000", "1003", "1006"): self.mouse = on
                continue
            if ch == "\r": self.c = 0
            elif ch == "\n": self.r = min(self.rows - 1, self.r + 1)
            elif ch >= " ":
                if self.c < self.cols: self.grid[self.r][self.c] = ch
                self.c += 1
            i += 1
    def text(self): return "\n".join("".join(row).rstrip() for row in self.grid)
    def has(self, s): return s in self.text()

class Session:
    def __init__(self, rows=30, cols=100):
        self.term = Term(rows, cols)
        env = dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor", USER="tester")
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.chdir(ROOT); os.execvpe(os.path.join(ROOT, "bin/neogenera"), ["neogenera", "--ui"], env)
        self.set_size(rows, cols)
    def set_size(self, rows, cols):
        fcntl.ioctl(self.fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
    def pump(self, timeout=0.3):
        end = time.time() + timeout
        while time.time() < end:
            r, _, _ = select.select([self.fd], [], [], 0.05)
            if r:
                try: data = os.read(self.fd, 65536)
                except OSError: return False
                if not data: return False
                self.term.feed(data)
        return True
    def wait_for(self, text, timeout=8):
        end = time.time() + timeout
        while time.time() < end:
            self.pump(0.2)
            if self.term.has(text): return True
        return False
    def send(self, data):
        os.write(self.fd, data.encode() if isinstance(data, str) else data)
    def locate(self, text):
        for r, row in enumerate(self.term.grid):
            line = "".join(row); c = line.find(text)
            if c >= 0: return r, c
        return None

failures = []
def check(name, ok):
    print(("PASS " if ok else "FAIL ") + name)
    if not ok: failures.append(name)

def main():
    s = Session(30, 100)
    check("window system starts (title, prompt, who-line)", s.wait_for("CL-USER>") and s.term.has("Listener") and s.term.has("Package:"))
    check("alternate screen and mouse reporting enabled", s.term.alt_screen and s.term.mouse)
    check("cursor is shown for the input line", s.term.cursor_visible is True)

    s.send("(+ 1 2)\r")
    check("evaluating (+ 1 2) shows => 3", s.wait_for("=> 3"))
    s.send("(expt 2 100)\r")
    check("bignum result displayed", s.wait_for("=> 1267650600228229401496703205376"))
    s.send("(car 5)\r")
    check("errors are reported in the pane", s.wait_for("Error: The value 5 is not of type LIST"))
    s.send("(format t \"hello~%\")\r")
    check("output goes to the pane", s.wait_for("hello"))

    # mouse: hover a result, then click it
    pos = s.locate("=> 3")
    if pos:
        r, c = pos
        s.send("\x1b[<35;%d;%dM" % (c + 4, r + 1))          # motion over the value (1-based coords)
        check("hover shows mouse documentation", s.wait_for("L: Describe"))
        s.send("\x1b[<0;%d;%dM\x1b[<0;%d;%dm" % (c + 4, r + 1, c + 4, r + 1))
        check("left click describes the object", s.wait_for("is of type fixnum"))
    else:
        check("found result to click", False)

    s.send(":help\r")
    check(":help lists commands", s.wait_for(":tile"))
    s.send("\x1b[19~")                                        # F8: next theme
    s.pump(0.5)
    s.send("(+ 20 22)\r")
    check("still responsive after a theme change", s.wait_for("=> 42"))

    # resize the terminal: the window system must relayout and keep the who-line on the last row
    s.set_size(20, 70); s.term.resize(20, 70)
    s.pump(1.0)
    ok = False
    end = time.time() + 6
    while time.time() < end and not ok:
        s.pump(0.3)
        ok = "Package:" in "".join(s.term.grid[19])
    check("resize relays out the screen (who-line on the new last row)", ok)
    s.send("(* 6 7)\r")
    check("works after resize", s.wait_for("=> 42"))

    # tab completion
    s.send("(defu\t")
    check("tab completes defun", s.wait_for("(defun"))
    s.send("\x15")                                           # C-u clears the line

    # leave with F10 and verify the terminal is restored
    s.send("\x1b[21~")
    deadline = time.time() + 6; exited = None
    while time.time() < deadline:
        s.pump(0.2)
        try:
            p, status = os.waitpid(s.pid, os.WNOHANG)
        except ChildProcessError:
            p, status = s.pid, 0
        if p: exited = status; break
    check("F10 exits cleanly", exited is not None and os.WIFEXITED(exited) and os.WEXITSTATUS(exited) == 0)
    raw = s.term.raw
    check("terminal state restored (alternate screen off, mouse off, cursor shown)",
          b"\x1b[?1049l" in raw and b"\x1b[?1000l" in raw and b"\x1b[?25h" in raw)
    if exited is None:
        try: os.kill(s.pid, signal.SIGKILL)
        except OSError: pass
    print("\n%d failure(s)" % len(failures))
    if failures:
        print("--- final screen ---"); print(s.term.text())
    sys.exit(1 if failures else 0)

if __name__ == "__main__":
    main()
