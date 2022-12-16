// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppThread.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rcpp_dists_to_n_targets
Rcpp::NumericMatrix rcpp_dists_to_n_targets(const Rcpp::DataFrame graph, const Rcpp::DataFrame vert_map_in, Rcpp::IntegerVector fromi, Rcpp::IntegerVector toi_in, const int n_targets);
RcppExport SEXP _utaengine_rcpp_dists_to_n_targets(SEXP graphSEXP, SEXP vert_map_inSEXP, SEXP fromiSEXP, SEXP toi_inSEXP, SEXP n_targetsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::DataFrame >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< const Rcpp::DataFrame >::type vert_map_in(vert_map_inSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type fromi(fromiSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type toi_in(toi_inSEXP);
    Rcpp::traits::input_parameter< const int >::type n_targets(n_targetsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_dists_to_n_targets(graph, vert_map_in, fromi, toi_in, n_targets));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_utaengine_rcpp_dists_to_n_targets", (DL_FUNC) &_utaengine_rcpp_dists_to_n_targets, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_utaengine(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}