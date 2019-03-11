L.avi <- function(data, avi, alts){

  # set the argument "avi"
  avi <- "alt.avi"

  # generate the alternative available columns
  avi_alts <- stringr::str_c(avi, alts, sep = ":")

  # set the alternative available columns 1, incidating all alternatives are
  # available to in all observations
  data[avi_alts] = 1

  # return the data set and the argument "avi"
  return(list(avi, data))
}
