#' @title S3 method for class 'dicmo'
#'
#' @author X.PAN
#'
#' @description summary results for class 'dicmo'
#'
#' @export
#'
#' @param object A 'dicmo' class object
#'
#' @param ... Other parameters that are not defined
#'
#' @method summary dicmo

summary.dicmo <- function(object, ...){
  res <- data.frame(coef. = object$Estimate,
                    std.err = object$std_err,
                    t.value = object$t_value,
                    p.value = object$p_value)
  res["signif."] <- " "
  res$signif.[which(object$p_value <= 0.01)] <- "***"
  res$signif.[which(object$p_value <= 0.05 & object$p_value > 0.01)] <- "**"
  res$signif.[which(object$p_value <= 0.1 & object$p_value > 0.05)] <- "*"
  names(res) <- c("estimate", "std. error", "t-value", "p-value", "")

  res[object$param_fixed, c(-1, -ncol(res))] <- NA
  res[object$param_fixed, ncol(res)] <- " "

  cat("---------------------------------------------------------", "\n")
  cat("Model name:", object$Name, "\n")
  cat("Model estimation starts at:", as.character(object$start_time), "\n")
  cat("Model estimation ends at:", as.character(object$end_time), "\n")
  cat("Model estimation method:", object$Type, "\n")
  cat("Model diagnosis:", object$Message, "\n")
  cat("---------------------------------------------------------", "\n")
  cat("Initial log-likelihood:", object$Initial_LL, "\n")
  cat("Convergent log-likelihood:", object$Final_LL, "\n")
  cat("Rho sqaured:", object$Rho_squared, "\n")
  cat("Adjusted Rho sqaured:", object$Adj_Rho_squared, "\n")
  cat("AIC:", object$AIC, "\n")
  cat("BIC:", object$BIC, "\n")
  cat("Sample size:", object$Sample_Size, "\n")
  cat("Number of estimated parameters:", object$Parameter_Number, "\n")
  cat("---------------------------------------------------------", "\n")
  cat("Estimates:", "\n")
  print(res, print.gap = 2)
  cat("------", "\n")
  cat("Signif. codes: *** p < 0.01, ** p < 0.05, * p < 0.1", "\n")
  cat("---------------------------------------------------------", "\n")
}
