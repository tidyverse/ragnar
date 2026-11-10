R CMD check: 0 errors, 0 warnings, 1 note.

The CRAN incoming feasibility check reports that the URL https://ror.org/03wc8by49, generated in `man/ragnar-package.Rd` from the ROR ID in `Authors@R`, returned HTTP 429 Too Many Requests. As a result, the ROR ID is reported as possibly invalid. The ID belongs to Posit Software, PBC and resolves via the ROR API at https://api.ror.org/v2/organizations/03wc8by49. The ror.org website rate-limits scripted requests.

This release fixes the installation errors on CRAN's Windows builders. On Windows, ragnar constrains ONNX Runtime to version 1.20.1 or earlier. Except on native Windows ARM64, installation attempts to warm the Python dependency cache in a separate process, so a native import failure during warm-up no longer aborts installation.

## Reverse dependencies

We checked 1 reverse dependency (commons), comparing R CMD check results across CRAN and dev versions of this package. We saw 0 new problems.