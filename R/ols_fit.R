#' Estimasi Regresi Linier Berganda
#'
#' Menghitung koefisien regresi menggunakan metode OLS dengan QR dekomposisi.
#'
#' @param X Matriks desain (dengan kolom pertama intercept).
#' @param y Vektor respons.
#'
#' @return List: koefisien, fitted values, residual, dan sigma2.
#' @export
#'
#' @examples
#' # Contoh data
#' set.seed(123)
#' n <- 100
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' y <- 3 + 2*x1 - x2 + rnorm(n, sd = 0.5)
#'
#' # Tambahkan kolom intercept
#' X <- cbind(1, x1, x2)
#'
#' # Fit model
#' model <- ols_fit(X, y)
#'
#' # Lihat hasil
#' model$coefficients
#' model$sigma2
#'
#' # Prediksi nilai baru
#' X_new <- cbind(1, c(0.5, -1), c(1.2, 0.3))
#' predict_ols(model, X_new)
ols_fit <- function(X, y) {
  X <- as.matrix(X)
  y <- as.matrix(y)

  beta_hat <- qr.solve(X, y)
  fitted <- X %*% beta_hat
  residuals <- y - fitted
  sigma2 <- sum(residuals^2) / (nrow(X) - ncol(X))

  return(list(
    coefficients = beta_hat,
    fitted.values = fitted,
    residuals = residuals,
    sigma2 = sigma2
  ))
}

#' Prediksi dengan Model OLS
#'
#' Prediksi nilai baru berdasarkan model OLS.
#'
#' @param model List dari hasil `ols_fit()`.
#' @param X_new Matriks desain baru.
#'
#' @return Vektor nilai prediksi.
#' @export
#'
#' @examples
#' # Lihat contoh di dokumentasi `ols_fit()`
predict_ols <- function(model, X_new) {
  X_new <- as.matrix(X_new)
  return(X_new %*% model$coefficients)
}
