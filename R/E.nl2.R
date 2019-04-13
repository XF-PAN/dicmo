E.nl2 <- function(process_data, param_start, alts, avi,
                  bw = NULL, scale = FALSE,
                  nest, choice, nest_uni,
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

  # get the name of attributes
  name_param <- names(df[, -1])

  # get the number of paramters/attributes to be estimated
  Nparam <- length(name_param)

  # set the initial values of parameters as 0
  beta <- rep(0, Nparam)

  # name the parameters
  names(beta) <- name_param

  # get the choice task id
  chid <- data$obs.id

  # get the number of alternative in the choice set
  Nalt <- length(alts)

  # get the number of observations
  Nobs <- nrow(df) / Nalt

  # nest structure setting
  nest.prop <- L.nest(data = data, nest = nest, choice = choice,
                      nest_uni = nest_uni, Nalt = Nalt, beta = beta)

  # get the updated parameters with iv parameters
  beta <- nest.prop[['beta']]

  # update the 'avi' argument
  if(is.null(avi)) avi <- "alt.avi"

  # this is only relevant for best-worst scaling case 3
  if(!is.null(bw)){

    if(bw){

      # inverse attributes' value for the worst preference
      x[((nrow(x)  / 2 + 1):nrow(x)), ] <- -x[((nrow(x)  / 2 + 1):nrow(x)), ]

      # get the model name
      model_name <- "exploded nested logit (best-worst only)"

    } else model_name <- "exploded nested logit"

  } else model_name <- "nested logit"

  # model estimation --------------------------------------------------------

  if(!scale){

    # re-set the initial values of certain parameters
    beta[names(param_start)] <- param_start

    start_time <- Sys.time()
    cat(as.character(start_time), "- model estimation starts\n")
    res <- maxLik::maxLik(logLik = logLik.nl2,
                          start = beta,
                          method = method,
                          fixed = param_fixed,
                          finalHessian = estimator,
                          control = list(iterlim = 1000),
                          attr = x, choice = y, chid = chid,
                          avi = as.matrix(data[avi]),
                          nest.alt = nest.prop[['nest.alt']],
                          nest.choice = nest.prop[['nest.choice']],
                          nest.id = nest.prop[['nest.id']],
                          nest.group = nest.prop[['nest.group']])
    end_time <- Sys.time()
    cat(as.character(end_time), "- model estimation ends\n")

  } else{

    if(bw){

      beta_scale <- c(scale.worst = 1)
      beta <- c(beta, beta_scale)

      scale_col <- sort(rep(c("scale.best", "scale.worst"), nrow(x) / 2))

    } else{

      Nscale <- length(alts) - 2
      beta_scale <- rep(1, Nscale)
      names(beta_scale) <- stringr::str_c("scale", 2:(Nscale + 1), sep = ".")
      beta <- c(beta, beta_scale)

      scale.last <- stringr::str_c("scale", Nscale + 2, sep = ".")
      scale_col <- sort(rep(c("scale.1", names(beta_scale), scale.last),
                            nrow(x) / length(alts)))

    }

    # re-set the initial values of certain parameters
    beta[names(param_start)] <- param_start

    start_time <- Sys.time()
    cat(as.character(start_time), "- model estimation starts\n")
    res <- maxLik::maxLik(logLik = logLik.nl2.scale,
                          start = beta,
                          method = method,
                          fixed = param_fixed,
                          finalHessian = estimator,
                          control = list(iterlim = 1000),
                          attr = x, choice = y, chid = chid,
                          Nparam = Nparam, scale_col = scale_col,
                          avi = as.matrix(data[avi]),
                          nest.alt = nest.prop[['nest.alt']],
                          nest.choice = nest.prop[['nest.choice']],
                          nest.id = nest.prop[['nest.id']],
                          nest.group = nest.prop[['nest.group']])
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
