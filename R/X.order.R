#' @title function to estimate ordered choice model
#'
#' @author X.PAN
#'
#' @description estimate ordered choice model
#'
#' @export X.order
#'
#' @importFrom rlang :=
#'
#' @param data A tibble, input data.
#'
#' @param choice A character, name of column indicating individuals' rate.
#'
#' @param rate A vector of characters, the scale that used ot rate an
#'     alternative.
#'
#' @param attrs A list contains three slices, whose elements have to be non-
#'     negative integers. The first one is a tibble, named "attrs_alts",
#'     indicating the alternative-specific attributes (excluding ASCs); the
#'     second one, named "asc", is a vector indicating the ASCs, and the third
#'     one, named "context", is a tibble indicating the context variables
#'     (includig individuals' socio-demographics). The column names in all
#'     tibbles represent the name of attributes, or context variables. All have
#'     same numbers of rows (elements) with the length of argument "alts". The
#'     element in tibbles/vector indicates if the attribute, constant or
#'     context variable is alternative-specific: "0" means the attribute,
#'     constaant or context variable is not available for the alternative
#'     (based on the sequence of alternative in the argument "alts"); except
#'     "0", if some elements in one column have a same value, then the
#'     corresponding alternatives have generic parameter in terms of this
#'     attribute, constant or context variable.
#'
#' @param attr_coding A vector of character, names of categorical attributes.
#'     Default = NULL, which means all attributes are continuous.
#'
#' @param attr_level A list of vector of character, representing the levels of
#'     all categorical attributes. The first element of a vector is set as base
#'     level, others are set as 1, 2, 3...in sequence. This argument is only
#'     relevent when argument "attr_coding" is not NULL. Default = NULL.
#'
#' @param interact A vector of character, name of attributes' interaction,
#'     connected by "*". Default = NULL.
#'
#' @param type A character, indicating which type of ordered model is used,
#'     either "logit" or "probit". Default = "logit".
#'
#' @param method A character, passed to the function maxLik() in "maxLik"
#'     package. It indicates the method used in maximum likelihood estimation.
#'     Default = "BFGS".
#'
#' @param estimator A argument in the function maxLik() from "maxLik"
#'     package. Here this argument could only be either TRUE or "BHHH"/"bhhh".
#'     Default = TRUE.
#'
#' @param param_fixed A vector of characters, passed to the function maxLik() in
#'     "maxLik" package. It indicates which parameters are fixed. Default = NULL.
#'
#' @param param_start A vector of numbers, passed to the function maxLik() in
#'     "maxLik" package. It indicages the initial values of parameters.
#'     Default = NULL.
#'

X.order <- function(data, choice, rate, attrs, attr_coding = NULL,
                    attr_level = NULL, interact = NULL, type = "logit",
                    method = "BFGS", estimator = TRUE,
                    param_fixed = NULL, param_start = NULL){

  # data preparation --------------------------------------------------------

  # to add an extra row for each choice task
  rate <- c(rate, "pos.inf")

  # data preparation and return the data set can be used and the utility formula
  process_data <- L.data(data = data, choice = choice, alts = rate,
                         attrs = attrs, attr_coding = attr_coding,
                         attr_level = attr_level, interact = interact,
                         avi = NULL, flag = "order")

  # get the data set
  data <- process_data[[1]]

  # get the utiity formula
  utility <- process_data[[2]]

  # thresholds setting
  process_data <- L.order(data = data, rate = rate, choice = choice)

  # update the data set by adding the thresholds
  data <- process_data[[1]]

  # to get the threshold columns
  threshold <- process_data[[2]]

  # manipulate the utility function
  df <- stats::model.frame(utility, data)
  y <- df[[1]]
  x <- as.matrix(df[, -1])
  name_param <- names(df[, -1])
  Nparam <- length(name_param)
  beta <- rep(0, Nparam)
  names(beta) <- name_param

  # manipulate the threshold function
  df <- stats::model.frame(threshold, data)
  x_thd <- as.matrix(df[, -1])
  name_param <- names(df[, -1])
  Nparam_thd <- length(name_param)
  beta_thd <- seq(1, Nparam_thd)
  names(beta_thd) <- name_param

  param_fixed <- c(param_fixed, "thd.0", name_param[length(name_param)])
  beta <- c(beta, beta_thd)

  beta[names(param_start)] <- param_start
  chid <- data$obs.id
  Nalt <- length(rate) - 1
  Nobs <- nrow(df) / 2
  indicator <- rep(c(-1, 1), Nobs)

  # get the model type
  if(type == "logit"){

    fun <- stats::plogis

  } else if(type == "probit"){

    fun <- stats::pnorm

  } else stop("Undefined model type!")

  # model estimation --------------------------------------------------------

  start_time <- Sys.time()
  cat(as.character(start_time), "- model estimation starts\n")
  res <- maxLik::maxLik(logLik = logLik.order,
                        start = beta,
                        method = method,
                        fixed = param_fixed,
                        finalHessian = estimator,
                        control = list(iterlim = 1000),
                        attr = x, attr_thd = x_thd, choice = indicator,
                        chid = chid, fun = fun,
                        Nparam = Nparam, Nparam_all = length(beta))
  end_time <- Sys.time()
  cat(as.character(end_time), "- model estimation ends\n")

  # goodness of fit and return it -------------------------------------------

  L.gof(res = res, Nalt = Nalt, Nobs = Nobs,
        Nparam = length(beta) - length(param_fixed),
        param_fixed = param_fixed,
        name = paste("ordered", type, sep = " "),
        flag = "order",
        start_time = start_time, end_time = end_time)
}
