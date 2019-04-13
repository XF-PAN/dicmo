L.explode <- function(data, choice, avi, bw, utility){

  # define the varibal "obs.id", otherwise when chect the package
  # there would be a NOTE.
  obs.id <- "obs.id"

  # get the row number of the un-exploded data
  Nrow <- nrow(data)

  # creat a new column to indicate the choice after exploding, initially put
  # the best preference in the column
  data <- dplyr::mutate(data, expld.ch = as.matrix(data[choice[1]]))

  if(!bw){ # for the complete ranked-preference to be exploded

    for(i in 1:(length(choice) - 1)){ # explode the data one by one

      # generate a temple data to save the exploded data set
      data_tmp <- data[1:Nrow, ]

      # update the choice for the (i+1)th preference
      data_tmp <- dplyr::mutate(data_tmp,
                                expld.ch = as.matrix(data_tmp[choice[i + 1]]))

      for(j in 1:i){ # this loop is to set the all previous preferred
                     # alternatives unavailable

        data_tmp[avi][which(data_tmp[choice[j]] == TRUE), ] <- 0

      }

      # update the obsvertation id for the current exploded data
      data_tmp <- dplyr::mutate(data_tmp,
                                obs.id = obs.id + nrow(data_tmp) * i)

      # combine the current exploded data to the previous ones
      data <- dplyr::bind_rows(data, data_tmp)

    }
  } else{ # for best-worst scaling

    # get the columen of worst preference
    worst_ptn <- length(choice)

    # create the exploded data for the worst preference and
    # update the choice for the worst preference as well
    data_tmp <- dplyr::mutate(data,
                              expld.ch = as.matrix(data[choice[worst_ptn]]))

    # set the best preference in the exploded worst data unavailable
    data_tmp[avi][which(data_tmp[choice[1]] == TRUE), ] <- 0

    # update the obsvertation id for the worst exploded data
    data_tmp <- dplyr::mutate(data_tmp,
                              obs.id = obs.id + nrow(data_tmp))

    # combine the worst exploded data to the best one
    data <- dplyr::bind_rows(data, data_tmp)

  }

  # get the names of attributes to be estimated
  formula_x <- labels(stats::terms(utility))

  # re-generate the utility function using the new choice column
  f_utility <- stats::reformulate(termlabels = formula_x, response = "expld.ch")

  # return the exploded data and the new utility function
  return(list(data, f_utility))

}
