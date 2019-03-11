L.coding <- function(data, attr_coding, attr_level){

  # Generate an all-zero matrix to put the coded data set of an attribute.
  data_coding <- matrix(0, nrow = nrow(data),
                        ncol = (length(attr_level) - 1))

  # Convert the matrix to a tibble.
  data_coding <- dplyr::as_tibble(data_coding)

  # Get the column names of the tibble.
  names(data_coding) <- stringr::str_c(names(attr_coding),
                                       seq(1: ncol(data_coding)), sep = ".lv")

  # Coding of the attribute.
  for(i in 1:ncol(data_coding)){

    # Set the base level as "0" or "-1" based on the method of coding. The
    # first level appeared in the argument "attr_level" is set as base level.
    if(attr_coding == "e"){

      data_coding[which(data == attr_level[1]), ] <- -1
    } else if(attr_coding == "d"){

      data_coding[which(data == attr_level[1]), ] <- 0
    }

    # Set (i+1)th level appeared in the argument "attr_level" as 1 in the
    # (i+1)th column of data set "data_coding".
    data_coding[which(data == attr_level[i + 1]), i] <- 1
  }

  # Return the coded data set of the attribute.
  return(data_coding)
}
