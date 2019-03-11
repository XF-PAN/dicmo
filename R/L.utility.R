L.utility <- function(choice, attrs, attr_coding,
                      name_non_specific, name_specific, interact){

  # get the alternative-specific attributes and context variables
  attrs_specific <- dplyr::bind_cols(attrs[["attrs_alts"]],
                                     attrs[["context"]])

  attrs_specific_tmp <- attrs_specific

  for(i in 1:length(attrs_specific_tmp)){

    Nattrs <- nrow(unique(attrs_specific[, i]))

    sum_attrs <- sum(attrs_specific[, i])

    name_attr <- names(attrs_specific[, i])

    if(Nattrs !=1 | sum_attrs == 0){

      attrs_specific_tmp <- dplyr::select(attrs_specific_tmp, -name_attr)
    }
  }

  name_attrs_non_specified <- names(attrs_specific_tmp)

  model_attr_coding <- NULL
  if(length(name_attrs_non_specified) != 0){

    for(j in 1:length(name_attrs_non_specified)){

      if(name_attrs_non_specified[j] %in% names(attr_coding)){

        model_attr_coding_tmp <-
          name_non_specific[stringr::str_detect(
            name_non_specific,
            stringr::str_c(name_attrs_non_specified[j], "."))]
      } else{

        model_attr_coding_tmp <- name_attrs_non_specified[j]
      }

      model_attr_coding <- c(model_attr_coding, model_attr_coding_tmp)
    }
  }

  formula_x <- c(name_specific, model_attr_coding, interact)

  f_utility <- stats::reformulate(termlabels = formula_x, response = choice)

  return(f_utility)
}
