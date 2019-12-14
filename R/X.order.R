#' @title function to estimate generalized ordered choice model
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
#' @param attrs A list contains only one slice, whose elements have to be positive
#'     integers. The only one slice is a tibble, named "attrs_alts",
#'     indicating the all attributes (including context variables). The column
#'     names in the tibbles represent the name of attributes. All have
#'     same numbers of rows (elements) with the length of argument "rate" minus
#'     one. The element in the tibble indicates if the attribute is
#'     alternative-specific: if some elements in one column have a same value,
#'     then the corresponding segments share a generic parameter in terms of this
#'     attribute.
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
#'     either "gologit" or "goprobit". Default = "gologit".
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
                    attr_level = NULL, interact = NULL, type = "gologit",
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

  # get the threshold set
  df <- stats::model.frame(threshold, data)

  # get the threshold column
  x_thd <- as.matrix(df[, -1])

  # get the names of thresholds
  name_param <- names(df[, -1])

  # get the number of thresholds
  Nparam_thd <- length(name_param)

  # set the initial values of thresholds
  beta_thd <- seq(1, Nparam_thd)

  # name the threshold parameters
  names(beta_thd) <- name_param

  # get all fixed parameters involving the -Inf and Inf thresholds

  param_fixed_all <- c("thd.0", name_param[length(name_param)], param_fixed,
                       names(beta)[stringr::str_detect(names(beta), "pos.inf")])

  # update the parameter to be estimated by adding the thresholds
  beta <- c(beta, beta_thd)

  beta["thd.0"] <- 1
  beta[name_param[length(name_param)]] <- 1

  # get the length of rating
  Nalt <- length(rate) - 1

  # get the number of observations
  Nobs <- nrow(df) / length(rate)

  # get the post and previous choice
  choice_pre <- df[ ,1]
  choice_post <- dplyr::lag(choice_pre)
  choice_post[1] <- FALSE

  # get the model type and the name of distributions to be used
  if(type == "gologit"){

    fun <- stats::plogis
    model_name <- "generalized ordered logit"

  } else if(type == "goprobit"){

    fun <- stats::pnorm
    model_name <- "generalized ordered probit"

  } else stop("Undefined model type!")

  # model estimation --------------------------------------------------------

  # re-set the initial values of certain parameters
  beta[names(param_start)] <- param_start

  start_time <- Sys.time()
  cat(as.character(start_time), "- model estimation starts\n")
  res <- maxLik::maxLik(logLik = logLik.order,
                        start = beta,
                        method = method,
                        fixed = param_fixed_all,
                        finalHessian = estimator,
                        control = list(iterlim = 1000),
                        attr = x, attr_thd = x_thd,
                        choice_pre = choice_pre, choice_post = choice_post,
                        chid = data$obs.id, fun = fun,
                        Nparam = Nparam, Nparam_all = length(beta))
  end_time <- Sys.time()
  cat(as.character(end_time), "- model estimation ends\n")

  # goodness of fit and return it -------------------------------------------

  L.gof(res = res, Nalt = Nalt, Nobs = Nobs,
        Nparam = length(beta) - length(param_fixed_all),
        param_fixed = param_fixed,
        param_fixed_all = param_fixed_all,
        name = model_name,
        flag = "order",
        start_time = start_time, end_time = end_time, estimator = estimator)

}
