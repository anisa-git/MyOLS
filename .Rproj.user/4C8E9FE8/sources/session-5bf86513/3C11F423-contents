#' Prediksi dengan Model OLS
#'
#' Prediksi nilai baru berdasarkan model OLS.
#'
#' @param model List dari hasil `ols_fit()`.
#' @param X_new Matriks desain baru.
#'
#' @return Vektor nilai prediksi.
#' @export
predict_ols <- function(model, X_new) {
  return(X_new %*% model$coefficients)
}
