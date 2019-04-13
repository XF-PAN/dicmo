logLik.logit.scale <- function(beta, attr, choice, chid,
                               Nparam, scale_col, avi){

  scale_col <- beta[scale_col]
  scale_col[is.na(scale_col)] <- 1

  exp_v <- exp((attr %*% beta[1:Nparam]) * scale_col) * avi
  p <- rowsum(exp_v * choice, chid) / rowsum(exp_v, chid)

  log(p)
}
