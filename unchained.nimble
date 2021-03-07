# Package

version       = "0.1.0"
author        = "Vindaar"
description   = "Fully type safe, compile time only units library"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.2.0"

task test, "Runs the test":
  exec "nim c -r tests/tunchained.nim"

task gen_docs, "Generate the docs":
  exec "nim doc --hints:off --warnings:off --git.url:https://github.com/SciNim/unchained --git.commit:master --git.devel:master -o:docs/unchained.html --index:on unchained.nim"
  # now build  the index
  exec "nim buildIndex -o:docs/theindex.html docs"
