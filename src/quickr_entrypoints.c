#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>


extern void snap_candidates_q(
  const int* const target__,
  const int* const candidates__,
  const int* const max_snap_dist__,
  int* const snap__,
  const R_len_t candidates__len_,
  const R_len_t target__len_);

SEXP snap_candidates_q_(SEXP _args) {
  // target
  _args = CDR(_args);
  SEXP target = CAR(_args);
  if (TYPEOF(target) != INTSXP) {
    Rf_error("typeof(target) must be 'integer', not '%s'", R_typeToChar(target));
  }
  const int* const target__ = INTEGER(target);
  const R_xlen_t target__len_ = Rf_xlength(target);

  // candidates
  _args = CDR(_args);
  SEXP candidates = CAR(_args);
  if (TYPEOF(candidates) != INTSXP) {
    Rf_error("typeof(candidates) must be 'integer', not '%s'", R_typeToChar(candidates));
  }
  const int* const candidates__ = INTEGER(candidates);
  const R_xlen_t candidates__len_ = Rf_xlength(candidates);

  // max_snap_dist
  _args = CDR(_args);
  SEXP max_snap_dist = CAR(_args);
  if (TYPEOF(max_snap_dist) != INTSXP) {
    Rf_error("typeof(max_snap_dist) must be 'integer', not '%s'", R_typeToChar(max_snap_dist));
  }
  const int* const max_snap_dist__ = INTEGER(max_snap_dist);
  const R_xlen_t max_snap_dist__len_ = Rf_xlength(max_snap_dist);

  if (max_snap_dist__len_ != 1)
    Rf_error("length(max_snap_dist) must be 1, not %0.f",
              (double)max_snap_dist__len_);
  const R_xlen_t snap__len_ = target__len_;
  SEXP snap = PROTECT(Rf_allocVector(INTSXP, snap__len_));
  int* snap__ = INTEGER(snap);

  snap_candidates_q(
    target__,
    candidates__,
    max_snap_dist__,
    snap__,
    candidates__len_,
    target__len_);

  UNPROTECT(1);
  return snap;
}

static const R_ExternalMethodDef QuickrEntries[] = {
  {"snap_candidates_q_", (DL_FUNC) &snap_candidates_q_, -1}
};

#include <R_ext/Rdynload.h>

void R_init_ragnar_quick_functions(DllInfo *dll) {
  R_registerRoutines(dll, NULL, NULL, NULL, QuickrEntries);
}
