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

  # select the row numbers that respondents choose and the row below
  row_part1 <- which(data[choice] == TRUE)
  row_part2 <- row_part1 + 1
  row_used <- sort(c(row_part1, row_part2))

  # get the rows according to the row numbers
  data <- data[row_used, ]

  # generate the threshold function
  threshold <- names(data)[stringr::str_detect(names(data), "thd.")]
  threshold <- stats::reformulate(termlabels = threshold, response = choice)

  # return the re-formed data set and the threshold function
  return(list(data, threshold))
}
