#' @title function to estimate exploded logit/probit model, including BWS case 3
#'
#' @author X.PAN
#'
#' @description This function could estimate multinomial logit model along with
#'     converting the data from a wide format to a long format and code the
#'     categorical attributesallow allow. In detail, it allows to estimate
#'     interaction effects between attributes and alternative-specific
#'     parameters.
#'
#' @export X.explode
#'
#' @importFrom rlang :=
#'
#' @param data A tibble, input data, wide format.
#'
#' @param choice A vector of character, name of column indicating individuals'
#'     choices from the most to the worst - the order matters! This argument is
#'     useless and not necessary if the argument "rank_alts" is TRUE.
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
#' @param type A character, indicating which type of ordered model is used,
#'     either "logit" or "nl2". Default = "logit".
#'
#' @param nest A list, indicating how many nests at all and which alternatives
#'     are in the same nest.
#'
#' @param nest_uni A logical, indicating if the inclusive values for the nests
#'     are the same or not. Default = TRUE, means the inclusive values are the
#'     same.
#'
#' @param bw A logical, if TRUE, then BWS case 3 is estimated, otherwise the
#'     complete rank is used. Default = FALSE.
#'
#' @param scale A logical, if TRUE, alternative-specific scale parametes are
#'     estimated, otherwise it is fixed to 1. Note only alternative-specific
#'     scale parameters are allowed. Default = FASLE.
#' @param rank_alts A logical, if TRUE, the alternatives are ranked otherwise
#'     the preferences are ranked. Default = FALSE.
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

X.explode <- function(data, choice, alts, attrs, attr_coding = NULL,
                      attr_level = NULL, interact = NULL,
                      type = "logit", nest, nest_uni =TRUE,
                      bw = FALSE, scale = FALSE, rank_alts = FALSE,
                      avi = NULL, method = "BFGS", estimator = TRUE,
                      param_fixed = NULL, param_start = NULL){

  # the following "if" is to convert rank-alternative data to
  # rank-preference data
  if(rank_alts){

    # set the name of rank-preference names
    choice <- stringr::str_c("pref", 1:length(alts), sep = ".")

    # creat the rank-prefernce columns
    for(i in 1:length(alts)){

      data <- dplyr::mutate(data, !!choice[i] := NA)

    }

    # convert rank-alternative data to rank-preference data row by row
    for(i in 1:nrow(data)){

      data[i, (ncol(data) - length(alts) + 1):ncol(data)] <-
        alts[order(as.numeric(data[i, 1:6]))]

    }

  }

  # get the sample size
  Sample_Size <- nrow(data)

  # data preparation and return the data set can be used and the utility formula
  process_data <- L.data(data = data, choice = choice, alts = alts,
                         attrs = attrs, attr_coding = attr_coding,
                         attr_level = attr_level, interact = interact,
                         avi = avi)

  # get the data set
  data <- process_data[[1]]

  # get the utiity formula
  utility <- process_data[[2]]

  # update the 'avi' argument
  if(is.null(avi)) avi <- "alt.avi"

  # explode the data set
  process_data <- L.explode(data = data, choice = choice, avi = avi,
                            bw = bw, utility = utility)

  # model estimation
  if(type == "logit"){

    res <- E.logit(process_data = process_data, bw = bw, scale = scale,
                   param_start = param_start, alts = alts, avi = avi,
                   method = method, param_fixed = param_fixed,
                   estimator = estimator)

  } else if(type == "nl2"){

    res <-   E.nl2(process_data = process_data, bw = bw, scale = scale,
                   param_start = param_start, alts = alts, avi = avi,
                   nest = nest, choice = "expld.ch", nest_uni = nest_uni,
                   method = method, param_fixed = param_fixed,
                   estimator = estimator)

  }

  # re-set the sample size
  res$Sample_Size <- Sample_Size

  # return the estimation results
  return(res)

}
