#' @title S3 method for class 'Xmnl'
#'
#' @author Xiaofeng Pan
#'
#' @description summary results for class 'Xmnl'
#'
#' @export
#'
#' @param object A 'Xmnl' class object
#'
#' @param ... Other parameters that are not defined
#'
#' @method summary Xmnl

summary.Xmnl <- function(object, ...){
  res <- data.frame(coef. = object$Estimate,
                    std.err = object$std_err,
                    t.value = object$t_value,
                    p.value = object$p_value)
  res["signif."] <- " "
  res$signif.[which(object$p_value <= 0.01)] <- "***"
  res$signif.[which(object$p_value <= 0.05 & object$p_value > 0.01)] <- "**"
  res$signif.[which(object$p_value <= 0.1 & object$p_value > 0.05)] <- "*"
  names(res) <- c(" estimate", " std. error", " t-value", " p-value", " ")

  cat("--------------------------------------------", "\n")
  cat("Initial log-likelihood:", object$Initial_LL, "\n")
  cat("Convergent log-likelihood:", object$Final_LL, "\n")
  cat("Rho sqaured:", object$Rho_squared, "\n")
  cat("Adjusted Rho sqaured:", object$Adj_Rho_squared, "\n")
  cat("AIC:", object$AIC, "\n")
  cat("BIC:", object$BIC, "\n")
  cat("Sample size:", object$Sample_Size, "\n")
  cat("Number of estimated parameters:", object$Parameter_Number, "\n")
  cat("--------------------------------------------", "\n")
  cat("Estimates:", "\n")
  print(res)
  cat("---", "\n")
  cat("Signif. codes: *** p < 0.01, ** p < 0.05, * p < 0.1", "\n")
  cat("--------------------------------------------", "\n")
}
