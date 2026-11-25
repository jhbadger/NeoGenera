#!/usr/bin/env python3
"""pty_test.py -- run `bin/neogenera --ui` in a pseudo-terminal and check what a user would see.

A small VT100/xterm emulator replays the exact bytes the program writes (cursor addressing,
clear, UTF-8 text; colours are ignored), so this validates the screen diffing, the input parser
(keys, mouse, resize) and terminal setup/restore against a real tty.
"""
import os, pty, sys, time, select, struct, fcntl, termios, signal, re, tempfile, shutil

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
    def __init__(self, rows=30, cols=100, home=None):
        self.term = Term(rows, cols)
        # NEOGENERA_HOME keeps the settings file away from the real ~/.neogenera
        env = dict(os.environ, TERM="xterm-256color", COLORTERM="truecolor", USER="tester",
                   NEOGENERA_HOME=home or tempfile.mkdtemp(prefix="ng-pty-"))
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

def quit_session(s):
    """Press F10 and wait for the process to exit; returns its wait status, or None if it did not."""
    s.send("\x1b[21~")
    deadline = time.time() + 6
    while time.time() < deadline:
        s.pump(0.2)
        try:
            p, status = os.waitpid(s.pid, os.WNOHANG)
        except ChildProcessError:
            p, status = s.pid, 0
        if p: return status
    try: os.kill(s.pid, signal.SIGKILL)
    except OSError: pass
    return None

def clean_exit(status):
    return status is not None and os.WIFEXITED(status) and os.WEXITSTATUS(status) == 0

def settings_persistence(home):
    path = os.path.join(home, "settings.lisp")
    check("plain use does not create a settings file", not os.path.exists(path))
    s = Session(30, 100, home)
    s.wait_for("CL-USER>")
    s.send(":set theme green\r")
    check(":set theme applies at once", s.wait_for("theme is now green"))
    s.send(":set tile grid\r")
    check(":set tile-mode accepts a name prefix", s.wait_for("tile-mode is now grid"))
    s.send(":save-settings\r")
    check(":save-settings reports the file", s.wait_for("Settings saved to"))
    s.send("(+ 40 2)\r")
    check("evaluates after saving", s.wait_for("=> 42"))
    s.send(":settings\r")
    check(":settings opens the Settings window", s.wait_for("NeoGenera Settings") and s.term.has("[Save settings]"))
    pos = s.locate("( ) amber")
    if pos:
        r, c = pos
        s.send("\x1b[<0;%d;%dM\x1b[<0;%d;%dm" % (c + 3, r + 1, c + 3, r + 1))   # click the amber choice
        check("clicking a value in the Settings window applies it", s.wait_for("(x) amber") and s.term.has("Theme *"))
    else:
        check("found the amber choice to click", False)
    check("quitting exits cleanly", clean_exit(quit_session(s)))
    text = open(path).read().lower() if os.path.exists(path) else ""
    check("the settings file holds the saved theme and tile mode", ":theme :green" in text and ":tile-mode :grid" in text)
    check("an unsaved change is not written at exit", ":amber" not in text)
    check("history is kept for the next session", "(+ 40 2)" in text)

    s = Session(30, 100, home)
    check("saved settings are loaded at startup", s.wait_for("CL-USER>"))
    s.send(":set theme\r")
    check("the saved theme is in effect", s.wait_for("theme is green"))
    s.send(":set tile-mode\r")
    check("the saved tile mode is in effect", s.wait_for("tile-mode is grid"))
    found = False
    for _ in range(8):                                         # Up past the newer lines until the saved one shows
        s.send("\x1b[A"); s.pump(0.3)
        if s.term.has("CL-USER> (+ 40 2)"): found = True; break
    check("saved history is recalled with Up", found)
    s.send("\x15")
    check("second session exits cleanly", clean_exit(quit_session(s)))

def main():
    home = tempfile.mkdtemp(prefix="ng-pty-")
    s = Session(30, 100, home)
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
    # the editor: open the source of MAPCAR from the Listener, edit, and close the window again
    s.send(":edit mapcar\r")
    check("editor opens at the definition of MAPCAR", s.wait_for("(defun mapcar (fn list &rest more)") and s.term.has("Editor:"))
    s.send("\x01XYZ")                                        # C-a then type: inserts at the start of the line
    check("typing edits the buffer (modified flag shown)", s.wait_for("XYZ(defun mapcar") and s.term.has("**"))
    s.send("\x1f")                                          # C-_ : undo
    check("undo reverts the edit", s.wait_for("--") )
    s.send("\x18\x03")                                      # C-x C-c: close the editor window
    s.pump(0.6)
    check("closing the editor returns to the Listener", s.wait_for("CL-USER>") and not s.term.has("Editor:"))
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
    final_screen = s.term.text()
    try: settings_persistence(home)
    finally: shutil.rmtree(home, ignore_errors=True)
    print("\n%d failure(s)" % len(failures))
    if failures:
        print("--- final screen ---"); print(final_screen)
    sys.exit(1 if failures else 0)

if __name__ == "__main__":
    main()
