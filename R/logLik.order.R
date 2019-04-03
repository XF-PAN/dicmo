logLik.order <- function(beta, attr, attr_thd, choice, chid, fun,
                         Nparam, Nparam_all){

  v <- attr %*% beta[1:Nparam]
  thd <- attr_thd %*% beta[(Nparam + 1):Nparam_all]
  p <- rowsum(fun(thd - v) * choice, chid)

  p[which(p < 0)] <- 0
  log(p)
}
