L.wide2long <- function(data, choice, alts, avi = NULL){

  # define the varibal "obs.id" and "alt.name", otherwise when chect the package
  # there would be NOTEs.
  obs.id <- "obs.id"
  alt.name <- "alt.name"

  # Generate a new column to indicate the id of observations.
  data <- dplyr::mutate(data, obs.id = seq(1, nrow(data)))

  # Get the column names that indicating alternative-specific attributes,
  # including the alternative-available column.
  attr_alts_speci <- names(data)[stringr::str_detect(names(data), ":")]

  # Generate the generic data part.
  data_generic <- dplyr::select(data, -attr_alts_speci)

  # The loop is to separate the alternative-specific data parts according to
  # alternatives and column-combine them with generic data part, then row-
  # combine them (based on the same colume names) to a long-format data set.
  data_wide <- NULL
  for(i in 1:length(alts)){

    # Get the column names that indicating alternative-specific attributes of
    # alternative i.
    data_alts_col_tmp <- attr_alts_speci[stringr::str_detect(attr_alts_speci,
                                                              alts[i])]

    # Separate the column names of those alternative-specific attributes based
    # on the symbol ":", which is the symbol to differentiate the alternative
    # and its specific attributes. the return result is a matrix.
    data_alts_col_name <-  stringr::str_split(data_alts_col_tmp, ":",
                                              simplify = TRUE)

    # Convert the above matrix to a vector with original sequence - with
    # alternative names in it.
    data_alts_col_name <- as.vector(t(data_alts_col_name))

    # Remove the alternative names in the vector.
    data_alts_col_name <- data_alts_col_name[
      !stringr::str_detect(data_alts_col_name, alts[i])]

    # Generate the specific data part of alternative i.
    data_alts_tmp <- dplyr::select(data, data_alts_col_tmp)

    # Rename the above specific data part with attribute name (e.g., without
    # alternative names).
    names(data_alts_tmp) <- data_alts_col_name

    # Generate a new column to indicate this specific data part belongs to
    # alternative i.
    data_alts_tmp <- dplyr::mutate(data_alts_tmp, alt.name = alts[i])

    # First column-combine the above specific data part with the generic data
    # part then row-combine it with previous specific & generic data part, where
    # the specific data part belongs to alternative i-1. the specific data part
    # of alternative 0 is pre-set to NULL.
    data_wide <- dplyr::bind_rows(data_wide,
                                  dplyr::bind_cols(data_generic,
                                                   data_alts_tmp))
  }

  # Set the NA value in the long-format data set to 0, where the NA is generated
  # in the case that some alternative-specific is not available to some certain
  # alternatives.
  data_wide[is.na(data_wide)] <- 0

  # Arrange the sequence of rows first based on the "obs.id" column then
  # "alt.name" column.
  data_wide <- dplyr::arrange(data_wide, obs.id, alt.name)

  # Re-set the "alt.name" columne as logic values to indicate if the
  # alternative is chosen.

  for(i in 1:length(choice)){

    data_wide[choice[i]] <- data_wide[choice[i]] == data_wide["alt.name"]
  }


  # Return the long-format data set.
  return(data_wide)
}
