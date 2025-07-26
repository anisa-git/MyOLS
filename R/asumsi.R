#' Uji Normalitas Residual Secara Manual
#'
#' Menggunakan skewness dan kurtosis (metode moment).
#'
#' @param model Hasil dari `ols_fit()`.
#' @return Pesan kesimpulan normalitas.
#' @export
normality <- function(model) {
  res <- model$residuals
  n <- length(res)
  mean_res <- mean(res)
  sd_res <- sd(res)

  # Hitung skewness
  skewness <- sum((res - mean_res)^3) / (n * sd_res^3)

  # Hitung kurtosis
  kurtosis <- sum((res - mean_res)^4) / (n * sd_res^4)

  cat("Skewness:", round(skewness, 3), "\n")
  cat("Kurtosis:", round(kurtosis, 3), "\n")

  if (abs(skewness) < 1 && abs(kurtosis - 3) < 1) {
    cat("✅ Residual cenderung normal.\n")
  } else {
    cat("❌ Residual tidak normal.\n")
  }

  # Visualisasi
  hist(res, main = "Histogram Residual", col = "lightblue", xlab = "Residuals")
  qqnorm(res); qqline(res, col = "red")
}

#' Uji Homoskedastisitas Secara Visual
#'
#' Pemeriksaan pola residual secara visual dan simpulan sederhana.
#'
#' @param model Hasil dari `ols_fit()`.
#' @export
homoskedasticity <- function(model) {
  res <- model$residuals
  fitted <- model$fitted.values

  plot(fitted, res,
       main = "Plot Residual vs Fitted",
       xlab = "Fitted", ylab = "Residuals",
       pch = 19, col = "darkgreen")
  abline(h = 0, col = "red", lty = 2)

  spread <- max(res) - min(res)
  if (spread < 4 * sd(res)) {
    cat("✅ Homoskedastisitas terlihat cukup baik.\n")
  } else {
    cat("❌ Ada indikasi heteroskedastisitas.\n")
  }
}

#' Uji Durbin-Watson Manual
#'
#' Menghitung statistik DW untuk mendeteksi autokorelasi.
#'
#' @param model Hasil dari `ols_fit()`.
#' @return Statistik DW dan interpretasi sederhana.
#' @export
autokorelasi <- function(model) {
  e <- as.vector(model$residuals)
  n <- length(e)

  numerator <- sum((e[2:n] - e[1:(n-1)])^2)
  denominator <- sum(e^2)
  dw <- numerator / denominator

  cat("Durbin-Watson:", round(dw, 3), "\n")

  if (dw < 1.5) {
    cat("❌ Ada indikasi autokorelasi positif.\n")
  } else if (dw > 2.5) {
    cat("❌ Ada indikasi autokorelasi negatif.\n")
  } else {
    cat("✅ Tidak ada indikasi autokorelasi.\n")
  }
}

#' VIF Manual
#'
#' Menghitung VIF tiap variabel prediktor secara manual.
#'
#' @param X Matriks desain (sudah tanpa kolom intercept).
#' @return Nilai VIF tiap kolom X.
#' @export
multikolinearitas <- function(X) {
  X <- as.matrix(X)
  k <- ncol(X)
  vif_values <- numeric(k)

  for (j in 1:k) {
    xj <- X[, j]
    x_other <- X[, -j]
    x_other <- cbind(1, x_other) # intercept

    beta <- qr.solve(x_other, xj)
    xj_hat <- x_other %*% beta
    residual <- xj - xj_hat
    r_squared <- 1 - (sum(residual^2) / sum((xj - mean(xj))^2))

    vif_values[j] <- 1 / (1 - r_squared)

    cat("VIF Variabel ke-", j, ": ", round(vif_values[j], 3), "\n")
    if (vif_values[j] > 10) {
      cat("❌ Ada indikasi multikolinearitas tinggi.\n")
    } else {
      cat("✅ Tidak ada indikasi multikolinearitas serius.\n")
    }
  }
  return(vif_values)
}
