#' @title function to estimate multinomial logit model
#'
#' @author Xiaofeng Pan
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
#' @param attrs A list contains two tibbles and a vector, whose elements are
#'     integers. The first one is a tibble represents the alternative-specific
#'     attributes (excluding ASCs), the secodn one is a vector represent the
#'     ASCs, and the third one is a tibble represents the context variables.
#'     The column names in all tibbles represent the name of attributes,
#'     constants or variables. All have same numbers of rows (elements) with
#'     the length of argument "alts". The element in tibbles/vector indicates
#'     if the attribute, constant or variable is alternative specific: "0"
#'     means the attribute, constaant or variable is set as base for the
#'     corresponding alternative; except "0", if some elements in one column
#'     have a same value, then the corresponding alternatives have generic
#'     parameter in terms of this attribute, constant or variable. The vector
#'     and second tibble in the list are defaultly set as NULL, which means no
#'     ASCs or context variables are taken into consideration.
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
#'     connected by "*", default = NULL.
#'
#' @param avi A character, name of column indicating if an alternative is
#'     available to respondents. Default = NULL, indicating all alternatives are
#'     available to all respondents. Each alternative should have such a column,
#'     for example, avi = "available" then the column's name for an alternative
#'     (e.g. the alternative's name is "car") should be "available_car". If this
#'     parameter is NULL, then those columns are not necessary. If this
#'     parameter is not NULL, then in such as column, the element should be 0 if
#'     the alternative is not available otherwise 1.
#'
#' @param opt_meth A character, passed to the function maxLik() in "maxLik"
#'     package. It indicates the method used in maximum likelihood estimation.
#'     Default = "BFGS".
#'
#' @param estimator A argument in the function maxLik() from "maxLik"
#'     package. Here this argument could only be either TRUE or "BHHH"/"bhhh".
#'     Default = TRUE.
#'
#' @param result A logical argument. If it is TRUE, the function return the
#'     estimated results, otherwise it return the data that directly entering
#'     the model. Default = TRUE.
#'

X.mnl <- function(data, choice, alts, attrs, attr_coding = NULL,
                  attr_level = NULL, interact = NULL, avi = NULL,
                  opt_meth = "BFGS", estimator = TRUE, result = TRUE){

  # data preparation and return the data set can be used and the utility formula
  process_data <- L.data(data, choice, alts, attrs, attr_coding,
                         attr_level, interact, avi)

  # get the data set
  data <- process_data[[1]]

  # get the utiity formula
  utility <- process_data[[2]]

  # if argument result = TRUE, do model estimation, otherwise return the data
  if(result){

    # model estimation --------------------------------------------------------

    df <- stats::model.frame(utility, data)
    y <- df[[1]]
    x <- as.matrix(df[, -1])
    name_param <- names(df[, -1])
    Nparam <- length(name_param)
    beta <- rep(0, Nparam)
    names(beta) <- name_param
    chid <- factor(data$obs.id)
    Nalt <- length(alts)
    Nobs <- nrow(df) / Nalt

    cat("Estimation starts at:", date(), "\n")
    res <- maxLik::maxLik(logLik = logLik.mnl,
                          start = beta,
                          method = opt_meth,
                          finalHessian = estimator,
                          control = list(iterlim = 1000),
                          attr = x, choice = y, chid = chid,
                          avi = as.matrix(data[avi]))
    cat("Estimation ends at:", date(), "\n")

    # goodness of fit and return it -------------------------------------------

    L.gof.mnl(res, Nalt, Nobs, Nparam,
               avi = as.matrix(data[avi]), chid = chid)
  }else{

    df <- stats::model.frame(utility, data)
    df <- as.matrix(cbind(data["obs.id"], df, data[avi]))
  }

}
