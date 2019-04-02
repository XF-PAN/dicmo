L.specific <- function(data, attrs, attr_coding, attr_level, alts, flag = NULL){

  # define the varibal "alt.name", otherwise when chect the package
  # there would be a NOTE.
  alt.name <- "alt.name"

  # get the firs colume of data to generate a tibble to put the alternative-
  # specific data part

  data_specific <- data[alt.name]

  # separate this each part of the argument "attrs"
  asc <- attrs[["asc"]]
  attrs_alts <- attrs[["attrs_alts"]]
  context <- attrs[["context"]]

  # for asc -----------------------------------------------------------------

  # get the number of values is asc
  Nasc <- length(unique(asc))

  # get the number of value 0 in asc, value 0 is set as base
  Nasc_0 <- length(asc[which(asc == 0)])

  # If there is no value 0 in asc and there are more than 1 element in asc, it
  # means no based alternative - the model is
  # over identified. If there is only 1 value 0 in asc,
  if(Nasc_0 == 0 & Nasc != 1){

    stop("Can not find the based alternative!")
  }

  # If there is value 0 in asc (maybe more than 1 value 0) and there are more
  # than 1 element in asc (i.e., there is at least 1 element except value 0),
  # then code the asc
  if(Nasc_0 != 0 & Nasc != 1){

    # This loop is to code the alternative according to the value of asc.
    for(i in 1:(Nasc - 1)){

      # get the vector that excluding 0
      unique_asc <- unique(asc)
      unique_asc <- unique_asc[unique_asc != 0]

      # code the non-zero value of asc
      if(unique_asc[i] != 0){

        # get the names of alternatives that have the same asc
        alts_same <- alts[which(asc == unique_asc[i])]

        # get the colume name of the asc
        col_name <- stringr::str_c("asc",
                                   stringr::str_c(alts_same, collapse = "."),
                                   sep = ".")

        # code the asc
        data_asc <- tibble::tibble(!!col_name :=
                                     as.vector(data$alt.name) %in% alts_same)

        # combined the coded data to the existed specific data part
        data_specific <- dplyr::bind_cols(data_specific, data_asc)
      }
    }
  }

  # for alternative-specific attribute --------------------------------------

  if(!is.null(asc)){

    # This loop is to deal with alternative-specific attributes one by one
    for(j in 1:length(attrs_alts)){

      # get the number of values for jth attribute
      Nattrs <- nrow(unique(attrs_alts[, j]))

      # get the number of values 0 for jth attribute
      # value 0 means the the corresponding alternative do not have jth attribute
      Nattrs_0 <- nrow(attrs_alts[which(attrs_alts[, j] == 0), j])

      # If the number of value for jth attribute is 1, it means this attribute has
      # generic parameter value.
      if(Nattrs != 1){

        for(i in 1:(Nattrs - sign(Nattrs_0))){

          # This "if" is for the categorical attribute that is already coded
          if(names(attrs_alts[, j]) %in% names(attr_coding)){

            # get the number of coded level of jth attribute
            Nlv <- length(attr_level[[names(attrs_alts[, j])]]) - 1

            # generate the name of the coded level of jth attribute
            lv_name <- stringr::str_c(names(attrs_alts[, j]), ".lv", seq(1, Nlv))

            unique_attrs_alts <- unique(attrs_alts[, j])
            unique_attrs_alts <- unique_attrs_alts[unique_attrs_alts != 0, ]
            # get the names of alternatives that have a generic parameter
            alts_same <- alts[which(attrs_alts[, j] ==
                                      as.numeric(unique_attrs_alts[i, ]))]

            # get the name for the new column for jth attribute
            col_name <- stringr::str_c(lv_name,
                                       stringr::str_c(alts_same, collapse = "."),
                                       sep = ".")

            # code the jth attribute level by level
            for(k in 1:Nlv){

              # code the kth level for jth attribute
              data_attr <- tibble::tibble(!!col_name[k] :=
                                            (data[lv_name] *
                                               (data$alt.name %in%
                                                  alts_same))[, k])

              # combined the coded data to the existed specific data part
              data_specific <- dplyr::bind_cols(data_specific, data_attr)
            }
          }

          # This "if" is for the continuous attribute
          if(!(names(attrs_alts[, j]) %in% names(attr_coding))){

            unique_attrs_alts <- unique(attrs_alts[, j])
            unique_attrs_alts <- unique_attrs_alts[unique_attrs_alts != 0, ]
            # get the names of alternatives that have a generic parameter
            alts_same <- alts[which(attrs_alts[, j] ==
                                      as.numeric(unique_attrs_alts[i, ]))]

            # get the name for the new column for jth attribute
            col_name <- stringr::str_c(names(attrs_alts[, j]),
                                       stringr::str_c(alts_same, collapse = "."),
                                       sep = ".")

            # code the jth attribute
            data_attr <- tibble::tibble(!!col_name :=
                                          (data[names(attrs_alts[, j])] *
                                             (data$alt.name %in% alts_same))[, 1])

            # combined the coded data to the existed specific data part
            data_specific <- dplyr::bind_cols(data_specific, data_attr)
          }
        }
      }
    }
  }

  # context attributes ------------------------------------------------------

  if(!is.null(context)){

    # This loop is to deal with contextual variables one by one
    for(j in 1:length(context)){

      # get the number of values for jth context
      Ncontext <- nrow(unique(context[, j]))

      # get the number of values 0 for jth context
      Ncontext_0 <- nrow(context[which(context[, j] == 0), j])

      # make sure context is not estimated using a generic parameter
      if(Ncontext == 1 & Ncontext_0 == 0){

        if(flag != "order"){

          stop("Impossible to estimate a genetric parameter of context variables!")
        }
      }

      if(Ncontext != 1){

        for(i in 1:(Ncontext - sign(Ncontext_0))){

          # This "if" is for the categorical context that is already coded
          if(names(context[, j]) %in% names(attr_coding)){

            # get the number of coded level of jth context
            Nlv <- length(attr_level[[names(context[, j])]]) - 1

            # generate the name of the coded level of jth context
            lv_name <- stringr::str_c(names(context[, j]), ".lv", seq(1, Nlv))

            unique_context <- unique(context[, j])
            unique_context <- unique_context[unique_context != 0, ]
            # get the names of alternatives that have a generic parameter
            context_same <- alts[which(context[, j] ==
                                         as.numeric(unique_context[i, ]))]

            # get the name for the new column for jth context
            col_name <- stringr::str_c(lv_name,
                                       stringr::str_c(context_same,
                                                      collapse = "."),
                                       sep = ".")

            # code the jth context level by level
            for(k in 1:Nlv){

              # code the kth level for jth context
              data_context <- tibble::tibble(!!col_name[k] :=
                                               (data[lv_name] *
                                                  (data$alt.name %in%
                                                     context_same))[, k])

              # combined the coded data to the existed specific data part
              data_specific <- dplyr::bind_cols(data_specific, data_context)
            }
          }

          # This "if" is for the continuous context
          if(!(names(context[, j]) %in% names(attr_coding))){

            unique_context <- unique(context[, j])
            unique_context <- unique_context[unique_context != 0, ]
            # get the names of alternatives that have a generic parameter
            context_same <- alts[which(context[, j] ==
                                         as.numeric(unique_context[i, ]))]

            # get the name for the new column for jth context
            col_name <- stringr::str_c(names(context[, j]),
                                       stringr::str_c(context_same,
                                                      collapse = "."),
                                       sep = ".")

            # code the jth context
            data_context <- tibble::tibble(!!col_name :=
                                             (data[names(context[, j])] *
                                                (data$alt.name %in%
                                                   context_same))[, 1])

            # combined the coded data to the existed specific data part
            data_specific <- dplyr::bind_cols(data_specific, data_context)
          }
        }
      }
    }
  }

  # Return the the specified data set.
  return(dplyr::select(data_specific, -alt.name))
}
