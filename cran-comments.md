R CMD check: 0 errors, 0 warnings, 0 notes.

This release fixes the installation errors on CRAN's Windows builders. ragnar now requires ONNX Runtime 1.20.1 or earlier on Windows, and installation warms the Python dependency cache in a separate process, so a native import failure no longer aborts installation.
