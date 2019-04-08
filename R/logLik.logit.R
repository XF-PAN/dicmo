logLik.logit <- function(beta, attr, choice, chid, avi){

  exp_v <- exp(attr %*% beta) * avi
  p <- rowsum(exp_v * choice, chid) / rowsum(exp_v, chid)

  log(p)
}
