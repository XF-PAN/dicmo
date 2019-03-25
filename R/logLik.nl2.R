logLik.nl2 <- function(beta, attr, choice, chid, avi,
                       nest.alt, nest.choice, nest.id,
                       nest.id.distinct, chid.distinct, nest.group){

  iv <- beta[nest.alt]
  iv[is.na(iv)] <- 1

  iv.distinct <- iv[!duplicated(nest.id)]

  exp_v <- exp((attr %*% beta[1:ncol(attr)] + log(avi)) / iv)
  exp_v_nest <- rowsum(exp_v * nest.choice, chid)

  numerator <- rowsum(exp_v * choice, chid) *
    exp_v_nest^(iv[choice == TRUE] - 1)

  denominator <- rowsum(rowsum(exp_v, nest.id)^iv.distinct, nest.group)

  p <- numerator / denominator

  log(p)
}
