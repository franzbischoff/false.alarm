#include "contrast.h"
#include "mpx.h"

/**
 * @brief Contrast Profile
 *
 * @param negative_data stream with the non-event
 * @param positive_data stream with the event
 * @param window_size window size
 * @param mp_time_constraint constraint
 * @param ez exclusion zone
 * @param s_size partial mode
 * @param idxs return the indexes?
 * @param euclidean use euclidean or pearson?
 * @param progress show progress?
 * @return List
 */

// [[Rcpp::export]]
List contrast_profile_rcpp(NumericVector negative_data, NumericVector positive_data, uint64_t window_size,
                           uint64_t mp_time_constraint, double ez, double s_size, bool idxs, bool euclidean,
                           bool progress) {

  // CP = (MP+- - MP++) / sqrt(2*window_size)

  List positive_mp = mpx_rcpp(positive_data, window_size, ez, mp_time_constraint, s_size, idxs, euclidean, progress);
  List ab_mp = mpxab_rcpp(positive_data, negative_data, window_size, s_size, idxs, euclidean, progress);

  NumericVector contrast;

  if (euclidean) {
    contrast = as<NumericVector>(ab_mp["matrix_profile"]) - as<NumericVector>(positive_mp["matrix_profile"]);
    contrast = contrast / sqrt(2 * window_size);
  } else {
    contrast = as<NumericVector>(positive_mp["matrix_profile"]) - as<NumericVector>(ab_mp["matrix_profile"]);
  }

  return (List::create(Rcpp::Named("positive_mp") = positive_mp, Rcpp::Named("ab_mp") = ab_mp,
                       Rcpp::Named("contrast_profile") = contrast, Rcpp::Named("w") = window_size,
                       Rcpp::Named("ez") = ez, Rcpp::Named("euclidean") = euclidean));
}
