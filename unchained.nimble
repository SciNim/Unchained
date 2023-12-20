# Package

version       = "0.3.10"
author        = "Vindaar"
description   = "Fully type safe, compile time only units library"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.6.0"

task installTestDeps, "Install dependencies to run tests in CI":
  exec "nimble install -y ggplotnim"

task test, "Runs the test":
  exec "nim c -r tests/tunchained.nim"
  exec "nim c -r tests/tresolveAlias.nim"
  when not defined(windows): # For some reason in CI in this repo lapack isn't found using msys2 (works in ggplotnim)
    exec "nim c -r examples/bethe_bloch.nim"
  exec "nim c -r examples/custom_unit_system.nim"

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
