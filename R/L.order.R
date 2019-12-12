L.order <- function(data, rate, choice){

  # this loop adds columns of thresholds, including -Inf and Inf
  for(i in 1:length(rate)){

    # get the name of threshold
    thd <- stringr::str_c("thd", (i - 1), sep = ".")

    # add the column
    if(i == 1){

      data <- dplyr::mutate(data, !!thd := log(1 * (data$alt.name != rate[i])))

    } else if(i == length(rate)){

      data <- dplyr::mutate(data, !!thd := -log(1 * (data$alt.name != rate[i])))

    } else{

      data <- dplyr::mutate(data, !!thd := 1 * (data$alt.name == rate[i]))

    }
  }

  # generate the threshold function
  threshold <- names(data)[stringr::str_detect(names(data), "thd.")]
  threshold <- stats::reformulate(termlabels = threshold, response = choice)

  # return the re-formed data set and the threshold function
  return(list(data, threshold))

}
