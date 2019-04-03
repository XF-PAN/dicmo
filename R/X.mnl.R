#' @title function to estimate multinomial logit model
#'
#' @author X.PAN
#'
#' @description This function could estimate multinomial logit model along with
#'     converting the data from a wide format to a long format and code the
#'     categorical attributesallow allow. In detail, it allows to estimate
#'     interaction effects between attributes and alternative-specific
#'     parameters.
#'
#' @export X.mnl
#'
#' @importFrom rlang :=
#'
#' @param data A tibble, input data, wide format.
#'
#' @param choice A character, name of column indicating individuals' choice.
#'
#' @param alts A vector of characters, names of all alternatives, including
#'     the none-option if any.
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
#' @param avi A character, name of column indicating if an alternative is
#'     available to individuals. Default = NULL, indicating all alternatives are
#'     available to all respondents. Each alternative should have such a column,
#'     for example, avi = "available" then the column's name for an alternative
#'     (e.g. the alternative's name is "car") should be "available:car" or
#'     "car:avilable". If this parameter is NULL, then those columns are not
#'     necessary. If this parameter is not NULL, then in such as column, the
#'     element should be 0 if the alternative is not available otherwise 1.
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

X.mnl <- function(data, choice, alts, attrs, attr_coding = NULL,
                  attr_level = NULL, interact = NULL, avi = NULL,
                  method = "BFGS", estimator = TRUE,
                  param_fixed = NULL, param_start = NULL){

  # data preparation --------------------------------------------------------

  # data preparation and return the data set can be used and the utility formula
  process_data <- L.data(data = data, choice = choice, alts = alts,
                         attrs = attrs, attr_coding = attr_coding,
                         attr_level = attr_level, interact = interact,
                         avi = avi)

  # get the data set
  data <- process_data[[1]]

  # get the utiity formula
  utility <- process_data[[2]]

  df <- stats::model.frame(utility, data)
  y <- df[[1]]
  x <- as.matrix(df[, -1])
  name_param <- names(df[, -1])
  Nparam <- length(name_param)
  beta <- rep(0, Nparam)
  names(beta) <- name_param
  beta[names(param_start)] <- param_start
  chid <- factor(data$obs.id)
  Nalt <- length(alts)
  Nobs <- nrow(df) / Nalt

  # update the 'avi' argument
  if(is.null(avi)) avi <- "alt.avi"

  # model estimation --------------------------------------------------------

  cat("Estimation starts at:", date(), "\n")
  res <- maxLik::maxLik(logLik = logLik.mnl,
                        start = beta,
                        method = method,
                        fixed = param_fixed,
                        finalHessian = estimator,
                        control = list(iterlim = 1000),
                        attr = x, choice = y, chid = chid,
                        avi = as.matrix(data[avi]))
  cat("Estimation ends at:", date(), "\n")

  # goodness of fit and return it -------------------------------------------

  L.gof(res = res, Nalt = Nalt, Nobs = Nobs,
        Nparam = length(beta) - length(param_fixed),
        param_fixed = param_fixed, avi = as.matrix(data[avi]),
        chid = chid)
}
