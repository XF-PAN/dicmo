L.gof <- function(res, Nalt, Nobs, Nparam, param_fixed, param_fixed_all = NULL,
                  avi = NULL, chid = NULL, name, flag = "nomial",
                  start_time, end_time, estimator){

  if(flag == "order"){ # for ordered choice

    # get the fixed parameters set by the userers
    param_fixed_NOthd <- param_fixed

    # get the first and last thresholds, which are -Inf and Inf, respectively -
    # they are fixed during the estimation
    param_fixed <- param_fixed_all[!(param_fixed_all %in% param_fixed)]

    # get the position of fixed thresholds
    id_fixed <- which(names(res$estimate) %in% param_fixed)

    # remove the rows and columns of the fixed thresholds in the hessian matrix
    hessian_mrx <- res$hessian[-id_fixed, -id_fixed]

    # remove the fixed thresholds in the estimated coefficients
    estimate <- res$estimate[-id_fixed]

  } else{ # for non-ordered choice

    # get the fixed parameters by the users
    param_fixed_NOthd <- param_fixed

    # get the hessian matrix
    hessian_mrx <- res$hessian

    # get the estimated coefficients
    estimate <- res$estimate

  }

  # compute the std. error
  se <- sqrt(diag(solve(-hessian_mrx)))

  # compute the t value
  t0 <- estimate / se

  # compute the p value
  p_value <- (1 - stats::pnorm(abs(t0))) * 2

  if(flag != "order"){ # compute the initial log-likelihood for
                       # non-ordered choice

    Initial_LL <- - sum(log(rowsum(avi, chid)))

  } else Initial_LL <- -Nobs * log(Nalt) # compute the initial log-likelihood
                                         # for ordered choice

  # compute the Rho squared
  Rho_squared <- 1 - res$maximum / Initial_LL

  # compute the adjusted Rho squared
  Adj_Rho_squared <- 1 - (res$maximum - Nparam) / Initial_LL

  # compute AIC index
  AIC <- 2 * Nparam - 2 * res$maximum

  # compute BIC index
  BIC <- log(Nobs) * Nparam - 2 * res$maximum

  # set the name of estimator
  if(estimator == TRUE){

    estimator = "with numeric Hessian matrix"

  }

  if(toupper(estimator) == "BHHH"){

    estimator = "with BHHH estimator"

  }

  # summary the resulsts and gof
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
                  BIC = round(BIC, 4),
                  Type = res$type,
                  Message = res$message,
                  Name = name,
                  start_time = start_time,
                  end_time = end_time,
                  param_fixed = param_fixed_NOthd,
                  estimator = estimator)

  # set class of the results
  class(results) <- "dicmo"

  # return the results
  return(results)

}
