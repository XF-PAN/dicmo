logLik.order.logis <- function(beta, attr, attr_thd, choice, chid,
                               Nparam, Nparam_all){

  v <- attr %*% beta[1:Nparam]
  thd <- attr_thd %*% beta[(Nparam + 1):Nparam_all]

  p <- rowsum(stats::plogis(thd - v) * choice, chid)

  p[which(p <= 0)] <- 1e-10
  log(p)
}
