# Windows release investigation, 2026-11-04

Branch: `fix/windows-release-blockers`, created from a fresh fetch of
`origin/main` at `d3b40c33899866404fca58a7c5068919ef500853`.
`DESCRIPTION` remains `0.3.1.9000`; no release branch was modified.

## CRAN installation failure (Windows x86_64)

The [CRAN summary](https://cran.rstudio.org/web/checks/check_results_ragnar.html)
still reports installation errors for released ragnar 0.3.1. All three detailed
logs were retrieved on November 4:

| Flavor | R | Installation log |
| --- | --- | --- |
| release | 4.6.1 (2026-06-24) | [00install](https://www.r-project.org/nosvn/R.check/r-release-windows-x86_64/ragnar-00install.html) |
| devel | 4.7.0 development, 2026-10-24 r90762 | [00install](https://www.r-project.org/nosvn/R.check/r-devel-windows-x86_64/ragnar-00install.html) |
| oldrel | 4.5.3 (2026-03-11) | [00install](https://www.r-project.org/nosvn/R.check/r-oldrel-windows-x86_64/ragnar-00install.html) |

All use `x86_64-w64-mingw32` on Windows Server 2022 x64, build 20348.
They initialize CPython 3.13.17 and NumPy 2.5.4, then report:

```text
./configure.win: line 2: ... Segmentation fault ... tools/configure_deps.R
ERROR: configuration failed for package 'ragnar'
```

The devel check is dated October 26; the summary refresh date does not mean
all individual checks ran November 4. The logs do not identify the native
module or stack that faulted, or give all Python package versions.

The configuration script is identical in v0.3.1 and the inspected main. It
eagerly calls `py_config()` and then `import("markitdown")` inside `try()`.
The output reaches the end of `py_config()` but never prints the imported
module or reaches the DuckDB warm-up. This places the failure at the native
Python import boundary; it does not establish which native dependency is
responsible. `try()` cannot catch termination of the R process.

Unmodified main installed locally with Python 3.12.15. Separate R subprocesses
also successfully imported MarkItDown with the exact CPython 3.13.17 standalone
build used by CRAN, and with 3.13.18. Both used NumPy 2.5.4, MarkItDown 0.1.9,
Magika 1.0.3, ONNX Runtime 1.31.1, and cryptography 50.0.2. Thus the native
segmentation fault itself was **not reproduced** on this machine. The CRAN
logs do not justify pinning or blaming a particular Python dependency.

### Fix and regression

Commit `fe97ad8` preserves Python cache warm-up, including the actual
MarkItDown import, but runs it in a separate R subprocess. Its output is
preserved and a nonzero exit status is reported explicitly before installation continues.
The existing separate-process DuckDB warm-up remains. Runtime conversion
requirements and errors are unchanged. No version constraint has been added.

`tools/test-install-warm-cache.R` exercises the public `R CMD INSTALL`
interface. Its isolated `PYTHONPATH` supplies a MarkItDown module that writes
a marker and calls `os._exit(86)`, modeling a native import terminating its
host process. The test requires that the import is attempted, the failure
is visible in the log, and installation succeeds. Removing the warm-up
therefore fails this test too. Subsequent `library(ragnar)` and public
`markdown_chunk()` assertions run without initializing Python.

The regression failed before the subprocess change because no warm-up was
attempted. Running it against the original `origin/main` also fails, because
the import terminates configuration. With the subprocess fix it passes and
reports `Python cache warm-up failed (exit status 86); continuing installation.`
An ordinary install still prints `Module(markitdown)` and succeeds. Windows
CI runs the regression. This tests the failure boundary; it is not a
reproduction of CRAN's unidentified native fault.

### MarkItDown version comparison

MarkItDown 0.1.9 (October 21) expanded its Magika requirement from
`~=0.6.1` to `>=0.6.1,<2`, as shown in
[upstream #2722](https://github.com/microsoft/markitdown/pull/2722).
On Windows x86_64 with Python 3.13.17, the compared environments were:

| MarkItDown | Magika | ONNX Runtime | NumPy | cryptography | Import and public HTML conversion |
| --- | --- | --- | --- | --- | --- |
| 0.1.8 | 0.6.3 | 1.20.1 | 2.5.4 | 50.0.2 | Pass |
| 0.1.9 | 1.0.3 | 1.31.1 | 2.5.4 | 50.0.2 | Pass |

The dependency change is a plausible lead for the CRAN crash, but neither
configuration reproduced it locally. A `markitdown<0.1.9` constraint cannot
yet be claimed to fix that crash.

Default uv resolution for native Windows ARM64/Python 3.13 selects
cryptography 50.0.2 with **both** MarkItDown 0.1.8 and 0.1.9. The older release
backtracks to Magika 0.6.2 and still selects ONNX Runtime 1.31.1 there.
Thus pinning MarkItDown alone does not fix the recorded cryptography build
failure. MarkItDown's own
[ARM64 CI fix](https://github.com/microsoft/markitdown/pull/2731) builds OpenSSL
and a native cryptography 50.0.2 wheel, rather than downgrading MarkItDown.

## R-universe timeout (native Windows ARM64)

Retrieved both current-main logs and the older job referenced by #213:

- [R-devel, job 126455909941](https://github.com/r-universe/tidyverse/actions/runs/41694454308/job/126455909941):
  R 4.7.0 development (2026-11-02 r90827), `aarch64-w64-mingw32`; tests start 08:07:35 UTC and the
  check is terminated at 09:06:46 UTC on November 4.
- [R-release, job 126455910139](https://github.com/r-universe/tidyverse/actions/runs/41694454308/job/126455910139):
  R 4.6.2; tests start 08:07:02 UTC and the check times out at 09:06:24 UTC.
- [Older job 117205272892](https://github.com/r-universe/tidyverse/actions/runs/38969288699/job/117205272892):
  R-devel r90676, DuckDB 1.5.6, also stops during tests after one hour.

The current jobs install DuckDB 2.0.0, reticulate 1.47.0, mirai 2.7.4,
nanonext 1.10.4, and testthat 3.3.2. Setup provides Python 3.13.17 ARM64;
reticulate downloads managed Python 3.13.18 ARM64. MarkItDown 0.1.9 brings
in pdfminer-six 20260107 and cryptography 50.0.2. The latter is built from
source and fails because its OpenSSL build dependency is unavailable.
The subsequent Python-requirements error is a consequence of that failure,
not evidence that the `<3.14` constraint itself is unsatisfiable.

The logs also show an HTTP 404 for DuckDB's `fts` extension. Direct requests
on November 4 confirmed 404 for **both** required extension binaries:

- <https://extensions.duckdb.org/v2.0.0/windows_arm64_mingw/fts.duckdb_extension.gz>
- <https://extensions.duckdb.org/v2.0.0/windows_arm64_mingw/vss.duckdb_extension.gz>

Cross-platform resolution with uv and `--python-platform aarch64-pc-windows-msvc
--python-version 3.13 --only-binary :all:` confirmed that cryptography 50.0.2
has no usable wheel. Without that exact version constraint, the same
MarkItDown requirements resolve using cryptography 46.0.3. Therefore the
observed default setup fails, but it would be incorrect to claim that no
Python environment can ever be assembled for Windows ARM64. No dependency
pin or unsupported DuckDB extension substitution was added.

### Identified initialization point; hang remains unverified

`ragnar_find_links()` retains Python's `collections.deque`, sets, and
dictionaries. Even the first local sitemap test calls
`reticulate::import("collections")`, which initializes the entire managed
Python environment, including the failing conversion requirements. This is
a concrete point to instrument in an ARM64 rerun. The function is unchanged
from `origin/main`; no link-discovery assertions were removed or weakened.

**The exact suspended native ARM64 stack remains unknown, and the one-hour
timeout is not established as fixed.** Neither ARM64 job published a
partial `testthat.Rout` or an ARM64 artifact, and their job logs contain no
per-test progress. Native ARM64 hardware was unavailable locally. Keep #213
open pending a native rerun with per-test progress; do not attribute the
timeout to the DuckDB 404, mirai, or a particular native module without that
evidence. The CRAN x86_64 installation crash remains a separate finding.

## Platform boundary and release implications

The complete Windows workflow is supported on x86_64 R. Native ARM64 DuckDB
indexing/retrieval remains blocked by missing upstream `fts` and `vss`
binaries. Link discovery and Python conversion on ARM64 also need a working
native Python dependency environment. Installation attempts both cache
warm-ups, reports failures, and continues. Successful installation does not
certify all package features on ARM64. This boundary is documented in
README.Rmd and README.md.

The freshly inspected main had passing R-universe x86_64 checks for
[release 4.6.2](https://github.com/r-universe/tidyverse/actions/runs/41694454308/job/126455911313),
[devel r90827](https://github.com/r-universe/tidyverse/actions/runs/41694454308/job/126455911656),
and [oldrel 4.5.3](https://github.com/r-universe/tidyverse/actions/runs/41694454308/job/126455911804),
all with managed Python 3.13.18. These are baseline results, not validation
of this branch. Repository Windows release CI also passed at the base commit.

Before releasing, rerun the fixed branch on CRAN-equivalent Windows x86_64
release/devel/oldrel, verify conversion on the affected CRAN environment,
and obtain a native ARM64 diagnostic run if closing the timeout issue is a
release requirement. Do not promise native ARM64 indexing/retrieval in 0.4.0.
The unrelated macOS findings from #213 and the remaining #215 release checklist
were not resolved by this investigation. The Atlas failure and check-directory
note below also need triage before calling Windows validation clean. No
submissions or GitHub writes were performed.

## Local environment and validation

Windows 11 x64 build 26300, Intel Core i7-8700K; R 4.6.1 UCRT,
`x86_64-w64-mingw32/x64`, Rtools45 GCC 14.3.0. R 4.4.3 and 4.6.0 are also
installed but were not used. No native ARM64, R 4.5.3, R 4.6.2, or R-devel
runtime was tested locally. Pandoc 3.13 was downloaded to the investigation
directory for vignette validation; no TeX installation was available.

| R dependency | Version |
| --- | --- |
| duckdb / DBI | 2.0.0 / 1.3.0 |
| reticulate / S7 | 1.47.0 / 0.2.2 |
| mirai / nanonext | 2.7.4 / 1.10.4 |
| testthat / pkgload / rcmdcheck | 3.3.2 / 1.5.3 / 1.4.0 |
| dplyr / httr2 / webfakes | 1.2.1 / 1.3.0 / 1.5.0 |
| nanoarrow / curl / xml2 / rvest | 0.9.0 / 8.1.0 / 1.6.0 / 1.0.5 |

MCP Console was used for R/Python exploration and launching isolated R
processes. Its inherited `R_LIBS` initially omitted the Console's current
mirai library, leaving a worker waiting for startup; that diagnostic run
was stopped. Explicitly propagating the current libraries fixed the harness.
Console also exports `UV_OFFLINE=1`; an initial broad baseline run therefore
failed the Atlas test when its uncached Python requirements were added
(missing `uvicorn`, one error and one warning). Validation children were
subsequently given `UV_OFFLINE=false`. These local harness problems are not
evidence about the ARM64 runner.

### Results

| Validation | Result |
| --- | --- |
| Entire `find-links` test group | 69 tests, 259 passing expectations, 0 failures/errors/warnings/skips, 78.55 seconds |
| Public `R CMD INSTALL` fault-injection regression | Exit 1 on original main and when warm-up is absent; exit 0 with subprocess fix, including attempted-import, diagnostic, load, and chunk assertions |
| Ordinary source installation, main and fixed | Both exit 0 locally |
| HTML conversion through `read_as_markdown()` | Passes with MarkItDown 0.1.8/Python 3.13.17 and MarkItDown 0.1.9/Python 3.13.17 and 3.13.18 |
| Full fixed non-CRAN suite, `UV_OFFLINE=false` | Reporter: **833 pass, 1 fail, 0 warnings, 7 skips**; 174 test blocks, 389.55 seconds |
| `R CMD check --as-cran --no-manual` | **0 errors, 0 warnings, 2 notes**, 3m 21.2s |
| Tests within that check | **426 pass, 0 fail, 0 warnings, 87 skips** under existing CRAN conditions |
| Check examples and vignette rebuild | Both OK |
| `git diff --check` | Clean |

The full-suite failure is `test-store-atlas.R`, including in a fresh isolated
run after resolving its dependencies. `ragnar_store_atlas()` imports
`embedding_atlas.projection` -> `embedding_atlas.embedding` ->
`embedding_atlas.utils` -> `inquirer.themes` -> `blessed.Terminal()`.
Terminal detection calls Windows `GetConsoleMode()` on an invalid handle and
raises `OSError: [WinError 6] The handle is invalid`. Versions: embedding-atlas
0.26.0, inquirer 3.4.1, blessed 1.52.0, jinxed 2.1.0, numba 0.68.1, llvmlite
0.50.0, Python DuckDB 2.0.0, uvicorn 0.54.0. This is an additional unresolved
Windows Atlas import failure, not evidence that the two investigated failures
share a cause. No terminal override, dependency pin, or skip was introduced
to make this test pass.

The full suite's seven existing skips are four unavailable provider
credentials, two MotherDuck tests, and an unavailable local Ollama service.
YouTube and other document-conversion tests ran. Pass counts in the table
are the test reporter's totals.

The check notes are the intentionally unchanged development version
`0.3.1.9000` and an unexpected empty directory literally named `'NULL'` in
`ragnar.Rcheck`. The latter was created around the start of examples; its
source was not established. The check used `NOT_CRAN=false` and
`_R_CHECK_CRAN_INCOMING_REMOTE_=false`; manual PDF generation was omitted
because TeX is unavailable. No claim of CRAN acceptance is implied.

Reproduction commands (with all DESCRIPTION dependencies installed, Pandoc
available, and an isolated installation for the ingestion workers):

```r
pkg <- pkgbuild::build(".", dest_path = tempdir(), vignettes = FALSE, manual = FALSE)
lib <- tempfile("ragnar-library-")
dir.create(lib)
install.packages(pkg, lib = lib, repos = NULL, type = "source")
.libPaths(c(lib, .libPaths()))
Sys.setenv(NOT_CRAN = "true", UV_OFFLINE = "false")
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
testthat::test_local(filter = "find-links", reporter = "summary", load_package = "installed")
testthat::test_local(reporter = "progress", load_package = "installed")

system2(file.path(R.home("bin"), "Rscript.exe"),
        c("tools/test-install-warm-cache.R", shQuote(pkg)))

rcmdcheck::rcmdcheck(
  args = c("--as-cran", "--no-manual"),
  env = c(NOT_CRAN = "false", `_R_CHECK_CRAN_INCOMING_REMOTE_` = "false"),
  error_on = "never"
)
```

For native ARM64 diagnosis, run the `find-links` group first in a fresh R
session, then the full suite with progress output captured. If a call still
stalls, capture its test name and stack before terminating it. A job-level
timeout with no test output is insufficient to mark the hang fixed.

Raw downloaded logs, dependency lists, test result RDS files, and build/check
outputs are retained locally under `%TEMP%/ragnar-windows-2026-11-04`.
