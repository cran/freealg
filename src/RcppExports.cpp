// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// lowlevel_simplify
List lowlevel_simplify(const List& words, const NumericVector& coeffs);
RcppExport SEXP _freealg_lowlevel_simplify(SEXP wordsSEXP, SEXP coeffsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type words(wordsSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type coeffs(coeffsSEXP);
    rcpp_result_gen = Rcpp::wrap(lowlevel_simplify(words, coeffs));
    return rcpp_result_gen;
END_RCPP
}
// lowlevel_free_prod
List lowlevel_free_prod(const List& words1, const NumericVector& coeffs1, const List& words2, const NumericVector& coeffs2);
RcppExport SEXP _freealg_lowlevel_free_prod(SEXP words1SEXP, SEXP coeffs1SEXP, SEXP words2SEXP, SEXP coeffs2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type words1(words1SEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type coeffs1(coeffs1SEXP);
    Rcpp::traits::input_parameter< const List& >::type words2(words2SEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type coeffs2(coeffs2SEXP);
    rcpp_result_gen = Rcpp::wrap(lowlevel_free_prod(words1, coeffs1, words2, coeffs2));
    return rcpp_result_gen;
END_RCPP
}
// lowlevel_free_sum
List lowlevel_free_sum(const List& words1, const NumericVector& coeffs1, const List& words2, const NumericVector& coeffs2);
RcppExport SEXP _freealg_lowlevel_free_sum(SEXP words1SEXP, SEXP coeffs1SEXP, SEXP words2SEXP, SEXP coeffs2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type words1(words1SEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type coeffs1(coeffs1SEXP);
    Rcpp::traits::input_parameter< const List& >::type words2(words2SEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type coeffs2(coeffs2SEXP);
    rcpp_result_gen = Rcpp::wrap(lowlevel_free_sum(words1, coeffs1, words2, coeffs2));
    return rcpp_result_gen;
END_RCPP
}
// lowlevel_free_power
List lowlevel_free_power(const List& words, const NumericVector& coeffs, const NumericVector& n);
RcppExport SEXP _freealg_lowlevel_free_power(SEXP wordsSEXP, SEXP coeffsSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const List& >::type words(wordsSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type coeffs(coeffsSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(lowlevel_free_power(words, coeffs, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_freealg_lowlevel_simplify", (DL_FUNC) &_freealg_lowlevel_simplify, 2},
    {"_freealg_lowlevel_free_prod", (DL_FUNC) &_freealg_lowlevel_free_prod, 4},
    {"_freealg_lowlevel_free_sum", (DL_FUNC) &_freealg_lowlevel_free_sum, 4},
    {"_freealg_lowlevel_free_power", (DL_FUNC) &_freealg_lowlevel_free_power, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_freealg(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}