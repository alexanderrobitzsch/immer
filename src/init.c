#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP immer_immer_sampling_xi(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP immer_probs_gpcm_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP immer_probs_gpcm_testlet_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP immer_probs_hrm_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP immer_sample_prob_index(SEXP, SEXP);
extern SEXP immer_subimmer_probs_gpcm_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP immer_subimmer_probs_gpcm_testlet_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP immer_subimmer_probs_hrm_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP immer_subimmer_sample_prob_index(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"immer_immer_sampling_xi",                (DL_FUNC) &immer_immer_sampling_xi,                12},
    {"immer_probs_gpcm_rcpp",                  (DL_FUNC) &immer_probs_gpcm_rcpp,                   6},
    {"immer_probs_gpcm_testlet_rcpp",          (DL_FUNC) &immer_probs_gpcm_testlet_rcpp,           7},
    {"immer_probs_hrm_rcpp",                   (DL_FUNC) &immer_probs_hrm_rcpp,                    6},
    {"immer_sample_prob_index",                (DL_FUNC) &immer_sample_prob_index,                 2},
    {"immer_subimmer_probs_gpcm_rcpp",         (DL_FUNC) &immer_subimmer_probs_gpcm_rcpp,          6},
    {"immer_subimmer_probs_gpcm_testlet_rcpp", (DL_FUNC) &immer_subimmer_probs_gpcm_testlet_rcpp,  7},
    {"immer_subimmer_probs_hrm_rcpp",          (DL_FUNC) &immer_subimmer_probs_hrm_rcpp,           6},
    {"immer_subimmer_sample_prob_index",       (DL_FUNC) &immer_subimmer_sample_prob_index,        2},
    {NULL, NULL, 0}
};

void R_init_immer(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
