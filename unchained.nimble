# Package

version       = "0.1.6"
author        = "Vindaar"
description   = "Fully type safe, compile time only units library"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.2.0"

task test, "Runs the test":
  exec "nim c -r tests/tunchained.nim"
  exec "nim c -r tests/tresolveAlias.nim"

task regressionTests, "Run regression tests (require cligen)":
  # NOTE: the following even compiled before, but took 10 GB of RAM. In a CI this will fail
  # for that reason, locally we just test it by hand
  exec "nim c -r tests/test_issue04_modified.nim"

import strformat
task gen_docs, "Generate the docs":
  let gitUrl = "https://github.com/SciNim/unchained"
  let masterBranch = "master"
  let defStr = "--hints:off --warnings:off"
  proc genFile(str: string, prefix = "") =
    exec &"nim doc {defStr} --git.url:{gitUrl} --git.commit:{masterBranch} --git.devel:{masterBranch} -o:docs/{str}.html --index:on src/{prefix}{str}.nim"
  #genFile("unchained")
  genFile("units", "unchained/")
  genFile("constants", "unchained/")
  genFile("utils", "unchained/")
  # now build  the index
  exec "nim buildIndex -o:docs/theindex.html docs"
