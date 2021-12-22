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

  List positive_mp =
      mpx_rcpp(positive_data, window_size, ez, mp_time_constraint, s_size, idxs = true, euclidean, progress);
  List ab_mp = mpxab_rcpp(positive_data, negative_data, window_size, s_size, idxs, euclidean, progress);

  NumericVector contrast;

  if (euclidean) {
    NumericVector mp_aa = as<NumericVector>(positive_mp["matrix_profile"]);
    NumericVector mp_ab = as<NumericVector>(ab_mp["matrix_profile"]);

    // Clip the MP above sqrt(2*w), these values are anti-correlated on pearson's space
    double clip = sqrt(2 * window_size);
    mp_ab[mp_ab > clip] = clip;
    mp_aa[mp_aa > clip] = clip;

    contrast = mp_ab - mp_aa;

    // Normalize between 0 and 1
    contrast = contrast / sqrt(2 * window_size);
    // negative values have no interest here.
    contrast[contrast < 0] = 0;

  } else {
    NumericVector mp_aa = as<NumericVector>(positive_mp["matrix_profile"]);
    NumericVector mp_ab = as<NumericVector>(ab_mp["matrix_profile"]);

    mp_ab[mp_ab < 0] = 0;
    mp_aa[mp_aa < 0] = 0;

    contrast = mp_aa - mp_ab;
    // negative values have no interest here.
    contrast[contrast < 0] = 0;
  }

  uint64_t plato_idx = which_max(contrast);
  uint64_t plato_nn_idx = as<IntegerVector>(positive_mp["profile_index"])[plato_idx] - 1; // idx was +1 for R interface
  NumericVector plato = positive_data[Range(plato_idx, plato_idx + window_size - 1)];
  NumericVector plato_nn = positive_data[Range(plato_nn_idx, plato_nn_idx + window_size - 1)];

  return (List::create(Rcpp::Named("contrast_profile") = contrast, Rcpp::Named("plato") = plato,
                       Rcpp::Named("plato_nn") = plato_nn, Rcpp::Named("plato_idx") = plato_idx + 1,
                       Rcpp::Named("plato_nn_idx") = plato_nn_idx + 1, Rcpp::Named("w") = window_size,
                       Rcpp::Named("ez") = ez, Rcpp::Named("euclidean") = euclidean));
}
