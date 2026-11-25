# Homebrew formula.  Copy to a tap (homebrew-neogenera/Formula/neogenera.rb) and fill in url and sha256 for the
# release tarball (the SHA256 of the GitHub source archive for the tag).
class Neogenera < Formula
  desc "Lisp environment evoking the Symbolics Genera workstations"
  homepage "https://example.invalid/neogenera"
  url "https://example.invalid/neogenera/archive/refs/tags/v1.0.0.tar.gz"
  sha256 "0000000000000000000000000000000000000000000000000000000000000000"
  license "MIT"
  head "https://example.invalid/neogenera.git", branch: "main"

  def install
    system "make", "world", "docs"
    system "make", "install", "PREFIX=#{prefix}"
  end

  test do
    assert_equal "1267650600228229401496703205376",
                 shell_output("#{bin}/neogenera --no-toplevel --eval '(princ (expt 2 100))'").strip
  end
end
