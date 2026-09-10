R CMD check: 0 errors, 0 warnings, 0 notes.

This release fixes the constructor-usage notes reported by CRAN. The Fedora clang error is due to the unavailable `duckdb` dependency. The Fedora gcc temporary-file note was not reproduced locally.
