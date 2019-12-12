logLik.order <- function(beta, attr, attr_thd, choice, chid_inf, chid, fun,
                         Nparam, Nparam_all){

  v <- attr %*% beta[1:Nparam]
  thd <- attr_thd %*% beta[(Nparam + 1):Nparam_all]

  p <- tapply(fun(thd - v), chid_inf, diff)
  p <- unlist(p)
  p <- rowsum(p * choice, chid)

  p[which(p < 0)] <- 0
  log(p)
}
