L.order <- function(data, rate, choice){

  for(i in 1:length(rate)){

    thd <- stringr::str_c("thd", (i - 1), sep = ".")

    if(i == 1){

      data <- dplyr::mutate(data, !!thd := log(1 * (data$alt.name != rate[i])))
    } else if(i == length(rate)){

      data <- dplyr::mutate(data, !!thd := -log(1 * (data$alt.name != rate[i])))
    } else{

      data <- dplyr::mutate(data, !!thd := 1 * (data$alt.name == rate[i]))
    }
  }

  row_part1 <- which(data[choice] == TRUE)
  row_part2 <- row_part1 + 1
  row_used <- sort(c(row_part1, row_part2))

  data <- data[row_used, ]

  threshold <- names(data)[stringr::str_detect(names(data), "thd.")]

  threshold <- stats::reformulate(termlabels = threshold, response = choice)

  return(list(data, threshold))
}
