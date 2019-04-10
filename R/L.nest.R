L.nest <- function(data, nest, choice, nest_uni, Nalt, beta){

  # add to columns to the tibble 'data', one is to indicate which alternative
  # belongs to which nest and the other is to indicate the id of nest across the
  # whole observations
  data <- dplyr::mutate(data,
                        nest.alt = 'none.nest',
                        nest.id = 0)

  # update the value in the 'nest.alt' column and the 'nest.id' column
  for(i in 1:length(nest)){

    data$'nest.alt'[data$alt.name %in% nest[[i]]] <-
      stringr::str_c('iv.', names(nest[i]))

    data$'nest.id'[data$alt.name %in% nest[[i]]] <- i
  }

  # get the name of the nest that containing the chosen alternative and expand
  # to the whole observation
  nest.choice <- data[data[choice] == TRUE, ]['nest.alt']
  # nest.choice <- dplyr::filter(data, get(choice) == TRUE)['nest.alt']
  nest.choice <- matrix(as.matrix(nest.choice),
                        nrow = nrow(nest.choice), ncol = Nalt)
  nest.choice <- as.vector(t(nest.choice))
  nest.choice <- nest.choice == data$nest.alt

  # update the 'nest.alt' column and beta according to whether a univeral
  # inclusive value is estimated
  if(nest_uni == FALSE){

    iv <- stringr::str_c('iv.', names(nest))
    beta[iv] <- 1
  } else{

    data$'nest.alt'[data$alt.name %in% unlist(nest)] <- 'iv'
    beta['iv'] <- 1
  }

  # update the nest id by adding obs.id
  nest.id <- stringr::str_c(data$obs.id, data$nest.id, sep = "-")

  # get the unique nest id
  nest.id.distinct = nest.id[!duplicated(nest.id)]

  # get the obs.id according to the unique nest id
  nest.group <- stringr::str_split(nest.id.distinct, pattern = '-',
                                   simplify = TRUE)[, 1]

  nest.prop <- list(nest.alt = data$nest.alt,
                    nest.choice = nest.choice,
                    nest.id = as.numeric(factor(nest.id)),
                    nest.group = as.numeric(factor(nest.group)),
                    beta = beta)
}
