L.explode <- function(data, choice, avi, bw, utility){

  # define the varibal "obs.id", otherwise when chect the package
  # there would be a NOTE.
  obs.id <- "obs.id"

  Nrow <- nrow(data)
  data <- dplyr::mutate(data, expld.ch = as.matrix(data[choice[1]]))

  if(!bw){

    for(i in 1:(length(choice) - 1)){

      data_tmp <- data[1:Nrow, ]

      data_tmp <- dplyr::mutate(data_tmp,
                                expld.ch = as.matrix(data_tmp[choice[i + 1]]))

      for(j in 1:i){

        data_tmp[avi][which(data_tmp[choice[j]] == TRUE), ] <- 0
      }

      data_tmp <- dplyr::mutate(data_tmp,
                                obs.id = obs.id + nrow(data_tmp) * i)

      data <- dplyr::bind_rows(data, data_tmp)

    }
  }

  if(bw){

    worst_ptn <- length(choice)

    # data_tmp <- data

    data_tmp <- dplyr::mutate(data,
                              expld.ch = as.matrix(data[choice[worst_ptn]]))

    data_tmp[avi][which(data_tmp[choice[1]] == TRUE), ] <- 0

    data_tmp <- dplyr::mutate(data_tmp,
                              obs.id = obs.id + nrow(data_tmp))

    data <- dplyr::bind_rows(data, data_tmp)

  }

  formula_x <- labels(stats::terms(utility))
  f_utility <- stats::reformulate(termlabels = formula_x, response = "expld.ch")

  return(list(data, f_utility))
}
