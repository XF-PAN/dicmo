L.gof <- function(res, Nalt, Nobs, Nparam, param_fixed,
                  avi = NULL, chid = NULL, flag = "nomial"){

  if(!is.null(param_fixed)){

    id_fixed <- which(names(res$estimate) %in% param_fixed)
    hessian_mrx <- res$hessian[-id_fixed, -id_fixed]
    estimate <- res$estimate[-id_fixed]
  } else{

    hessian_mrx <- res$hessian
    estimate <- res$estimate
  }

  se <- sqrt(diag(solve(-hessian_mrx)))
  t0 <- estimate / se
  p_value <- (1 - stats::pnorm(abs(t0))) * 2

  if(flag != "order"){

    Initial_LL <- - sum(log(rowsum(avi, chid)))
  } else{

    Initial_LL <- -Nobs * log(Nalt)
  }


  Rho_squared <- 1 - res$maximum / Initial_LL
  Adj_Rho_squared <- 1 - (res$maximum - Nparam) / Initial_LL
  AIC <- 2 * Nparam - 2 * res$maximum
  BIC <- log(Nobs) * Nparam - 2 * res$maximum

  results <- list(Hessian = hessian_mrx,
                  Estimate = round(estimate, 6),
                  std_err = round(se, 6),
                  t_value = round(t0, 4),
                  p_value = round(p_value, 4),
                  Initial_LL = round(Initial_LL, 3),
                  Final_LL = round(res$maximum, 3),
                  Rho_squared = round(Rho_squared, 4),
                  Adj_Rho_squared = round(Adj_Rho_squared, 4),
                  Sample_Size = Nobs,
                  Parameter_Number = Nparam,
                  AIC = round(AIC, 4),
                  BIC = round(BIC, 4))
  class(results) <- "dicmo"

  return(results)
}
