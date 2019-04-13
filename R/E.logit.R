E.logit <- function(process_data, param_start, alts, avi,
                    bw = NULL, scale = FALSE,
                    method, param_fixed, estimator){

  # get the data set
  data <- process_data[[1]]

  # get the utiity formula
  utility <- process_data[[2]]

  # get the data that needs to model estimation
  df <- stats::model.frame(utility, data)

  # get respondents' choice
  y <- df[[1]]

  # get the attributes columns
  x <- as.matrix(df[, -1])

  # get the names of attributes
  name_param <- names(df[, -1])

  # get the number of paramters/attributes to be estimated
  Nparam <- length(name_param)

  # set the initial values of parameters as 0
  beta <- rep(0, Nparam)

  # name the parameters
  names(beta) <- name_param

  # get the choice task id
  chid <- data$obs.id

  # get the number of alternatives in the choice set
  Nalt <- length(alts)

  # get teh number of observations
  Nobs <- nrow(df) / Nalt

  # update the 'avi' argument
  if(is.null(avi)) avi <- "alt.avi"

  # this is only relevant for best-worst scaling case 3
  if(!is.null(bw)){

    if(bw){

      # inverse attributes' value for the worst preference
      x[((nrow(x)  / 2 + 1):nrow(x)), ] <- -x[((nrow(x)  / 2 + 1):nrow(x)), ]

      # get the model name
      model_name <- "exploded logit (best-worst only)"

    } else model_name <- "exploded logit" # get the name of model to be estimated

  } else model_name <- "logit" # get the name of model to be estimated

  # model estimation --------------------------------------------------------

  # this "if-else" is for the exploded model with scale parameters to be
  # estimated
  if(!scale){ # no scale parameters

    # re-set the initial values of certain parameters
    beta[names(param_start)] <- param_start

    start_time <- Sys.time()
    cat(as.character(start_time), "- model estimation starts\n")
    res <- maxLik::maxLik(logLik = logLik.logit,
                          start = beta,
                          method = method,
                          fixed = param_fixed,
                          finalHessian = estimator,
                          control = list(iterlim = 1000),
                          attr = x, choice = y, chid = chid,
                          avi = as.matrix(data[avi]))
    end_time <- Sys.time()
    cat(as.character(end_time), "- model estimation ends\n")

  } else{ # scale parameters to be estimated

    if(bw){ # best-worst scaling is the model to be estimated

      # set the initial scale value for the worst preference
      beta_scale <- c(scale.worst = 1)

      # combine the scale parameter to other parameters
      beta <- c(beta, beta_scale)

      # generate the scale column
      scale_col <- sort(rep(c("scale.best", "scale.worst"), nrow(x) / 2))

    } else{ # the whole ranked preference is the model to be estimated

      # get the number of scale parameters to be estimated
      Nscale <- length(alts) - 2

      # set the initial values of scale parameters
      beta_scale <- rep(1, Nscale)

      # get the name of scale parameters
      names(beta_scale) <- stringr::str_c("scale", 2:(Nscale + 1), sep = ".")

      # combine the scale parameters to other parameters
      beta <- c(beta, beta_scale)

      # creat the name of the scale of worst-preference
      scale.last <- stringr::str_c("scale", Nscale + 2, sep = ".")

      # generate the scale column
      scale_col <- sort(rep(c("scale.1", names(beta_scale), scale.last),
                            nrow(x) / length(alts)))

    }

    # re-set the initial values of certain parameters
    beta[names(param_start)] <- param_start

    start_time <- Sys.time()
    cat(as.character(start_time), "- model estimation starts\n")
    res <- maxLik::maxLik(logLik = logLik.logit.scale,
                          start = beta,
                          method = method,
                          fixed = param_fixed,
                          finalHessian = estimator,
                          control = list(iterlim = 1000),
                          attr = x, choice = y, chid = chid,
                          Nparam = Nparam, scale_col = scale_col,
                          avi = as.matrix(data[avi]))
    end_time <- Sys.time()
    cat(as.character(end_time), "- model estimation ends\n")

  }

  # goodness of fit and return it -------------------------------------------

  L.gof(res = res, Nalt = Nalt, Nobs = Nobs,
        Nparam = length(beta) - length(param_fixed),
        param_fixed = param_fixed, avi = as.matrix(data[avi]),
        chid = chid,
        name = model_name,
        start_time = start_time, end_time = end_time)

}
