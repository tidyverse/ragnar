#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP pick_cut_positions_(SEXP candidate_positions, SEXP max_chunk_size_sexp) {
  // candidate positions is a sorted (increasing) integer vector of candidate
  // cut positions. i.e., char positions of sentence boundaries.

  // max_chunk_size is a scalar integer, the max desired difference between
  // selected candidate positions.

  // This function always selects the first and last candidate positions
  // (assumed to be 1 and nchar(string)), and then a subset of the remaining
  // candidates (assumed to be semantic boundaries safe to cut at). For each
  // selected cut position, it scans ahead in the consecutive candidates until
  // the a candidate that produces a chunk that is larger than max_chunk_size.
  // It then selects the previous candidate as the next cut point. This
  // potentially results in chunks that are larger than max_chunk_size, if there
  // are insufficient candidate boundaries.

  // Some not-so-great things about this implementation:
  // - the last chunk might be much disproportionately smaller than the rest. It
  //   would be nice if we "balanced" the chunks better, instead of greedily
  //   making the chunks as large as possible.
  // - supporting overlapping chunks would be nice. Implementing overlap here
  //   would be relatively straighforward, but it would introduce a lot of
  //   complexity in the whole package, since we would then want to support
  //   de-overlapping consecutive chunks on retreival.
  if (!Rf_isInteger(candidate_positions)) {
    Rf_error("`candidate_positions` must be an integer vector");
  }

  if (!Rf_isInteger(max_chunk_size_sexp) ||
      Rf_length(max_chunk_size_sexp) != 1) {
    Rf_error("`chunk_size` must be a scalar integer");
  }

  int max_chunk_size = Rf_asInteger(max_chunk_size_sexp);
  if (max_chunk_size <= 0) {
    Rf_error("`chunk_size` must be positive");
  }

  R_xlen_t n_candidates = Rf_xlength(candidate_positions);
  if (n_candidates <= 1) {
    return candidate_positions;
  }

  int *candidates = INTEGER(candidate_positions);
  SEXP result = PROTECT(Rf_allocVector(INTSXP, n_candidates));
  int *selected_cuts = INTEGER(result);
  int n_selected = 0;

  // Always include first candidate as starting point
  int last_cut = candidates[0];
  selected_cuts[n_selected++] = last_cut;

  R_xlen_t i = 1;
  while (i < n_candidates) {
    int candidate = candidates[i];
    int chunk_length = candidate - last_cut;

    if (chunk_length > max_chunk_size) {
      int previous_candidate = candidates[i - 1];
      if (previous_candidate != last_cut) {
        selected_cuts[n_selected++] = previous_candidate;
        last_cut = previous_candidate;
        // Don't advance i here since we need to recheck current candidate
        continue;
      }
    }
    i++; // Only advance if we didn't pick the candidate
  }

  // Add final candidate as last cut point if different from last cut
  if (candidates[n_candidates - 1] != last_cut) {
    selected_cuts[n_selected++] = candidates[n_candidates - 1];
  }

  SEXP final_result = PROTECT(Rf_lengthgets(result, n_selected));
  UNPROTECT(2);
  return final_result;
}

// Boilerplate to register native routine
static const R_CallMethodDef CallEntries[] = {
    {"pick_cut_positions_", (DL_FUNC)&pick_cut_positions_, 2}, {NULL, NULL, 0}};

void R_init_ragnar(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
