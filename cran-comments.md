Bug fixes and compatibility updates. See NEWS.md for details.

## R CMD check results

0 errors | 0 warnings | 0 notes

Checked on macOS with R 4.6.1, using both S7 0.2.2 and development S7 0.2.2.9000. Both runs passed 140 test expectations, with 6 tests skipped because credentials or external services were unavailable.

## Current CRAN results

The missing-usage notes for `MarkdownDocument` and `MarkdownDocumentChunks` are addressed by adding constructor usage documentation.

The Fedora clang error is due to the unavailable `duckdb` dependency. The Fedora gcc temporary-file note was not reproduced locally.
