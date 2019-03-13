L.data <- function(data, choice, alts, attrs, attr_coding,
                   attr_level, interact, avi){

  # input data format check -------------------------------------------------

  if(!dplyr::is.tbl(data)) stop("The input data is not a tibble!")

  # data process - alternative available or not -----------------------------

  if(is.null(avi)){

    process_data <- L.avi(data = data, avi = avi, alts = alts)
    avi <- process_data[[1]]
    data <- process_data[[2]]
  }

  # data transformation - wide format to long format ------------------------

  data <- L.wide2long(data = data, choice = choice, alts = alts, avi = avi)

  # data processing - attribute coding --------------------------------------

  # Generate a prior data set for iteration, if no categorical, then set as NULL
  data_coding <- NULL

  if(!is.null(attr_coding)){

    # This loop is to generate coded data set of attributes one by one and
    # column-combined together.
    for(i in 1:length(attr_coding)){

      # Generate coded data set of (i)th attribute appeared in the argument
      # "attr_coding" and column-combined to the previous one.
      data_coding <- dplyr::bind_cols(data_coding,
                                      L.coding(
                                        data = data[names(attr_coding[i])],
                                        attr_coding = attr_coding[i],
                                        attr_level =
                                          attr_level[[names(attr_coding[i])]]))
    }
  }

  # select the data part that is not coded
  data <- dplyr::select(data, setdiff(names(data), names(attr_coding)))

  # combined the non-coded data part and the coded data part
  data <- dplyr::bind_cols(data, data_coding)

  # data processing - specific attribute ------------------------------------

  # this function return the data that is specified based on alternatives
  data_specific <- L.specific(data = data, attrs = attrs,
                              attr_coding = attr_coding,
                              attr_level = attr_level, alts = alts)

  # get the alternative-specific attributes and context variables
  attrs_specific <- dplyr::bind_cols(attrs[["attrs_alts"]],
                                     attrs[["context"]])

  # This loop is to remove the column that is specified from the tibble "data".
  # Thus after the loop, the tibble "data" only contains the columns that are
  # not specified.
  for(i in 1:length(attrs_specific)){

    # get number that needs to be specificed
    Nattrs <- nrow(unique(attrs_specific[, i]))

    # # Get the sum of value from the argument "attrs_specific". If the sum is 0,
    # # it means the attribute does not appear in any alternative, namely, the
    # # attribute is not considered in the model.
    # sum_attrs <- sum(attrs_specific[, i])

    # get the attribute name
    name_attr <- names(attrs_specific[, i])

    # remove the column from the tibble "data" on the condition that if the
    # attribute is categorical or continuous
    if(Nattrs != 1){

      # if the attribute is categorical and coded and else the attribute is
      # continuous
      if(name_attr %in% names(attr_coding)){

        # get the column name that needs to be removed
        name_attr <- names(data)[stringr::str_detect(names(data),
                                                     stringr::str_c(name_attr,
                                                                    "."))]

        # remove the column from the tibble "data"
        data <- dplyr::select(data, -name_attr)
      } else{

        # # get the column name that needs to be removed
        # name_attr <- names(data)[stringr::str_detect(names(data), name_attr)]

        # remove the column from the tibble "data"
        data <- dplyr::select(data, -name_attr)
      }
    }
  }

  # get the column names of non-specified data
  name_non_specific <- names(data)

  # get the column names of specified data
  name_specific <- names(data_specific)

  # combined the non-specified data part and the specified data part
  data <- dplyr::bind_cols(data, data_specific)

  # data process - formula generation ---------------------------------------

  utility <- L.utility(choice = choice, attrs = attrs,
                       attr_coding = attr_coding,
                       name_non_specific = name_non_specific,
                       name_specific = name_specific, interact = interact)

  # return the data set and the utility formula
  return(list(data, utility))
}
