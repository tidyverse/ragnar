R CMD check: 0 errors, 0 warnings, 0 notes.

This release fixes the installation errors on CRAN's Windows builders. On Windows, ragnar constrains ONNX Runtime to version 1.20.1 or earlier. Except on native Windows ARM64, installation attempts to warm the Python dependency cache in a separate process, so a native import failure during warm-up no longer aborts installation.
