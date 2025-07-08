#' Estimasi Regresi Linier Berganda
#'
#' Menghitung koefisien regresi menggunakan metode OLS.
#'
#' @param X Matriks desain, kolom pertama biasanya adalah 1 (intercept).
#' @param y Vektor respons.
#'
#' @return List: koefisien, fitted values, residual, dan sigma2.
#' @export
ols_fit <- function(X, y) {
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
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
