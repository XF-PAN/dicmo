logLik.order <- function(beta, attr, attr_thd, choice_pre, choice_post,
                         chid, fun, Nparam, Nparam_all){

  v <- attr %*% beta[1:Nparam]
  thd <- attr_thd %*% beta[(Nparam + 1):Nparam_all]
  p <- fun(thd - v)
  p <- rowsum(p * choice_post, chid) - rowsum(p * choice_pre, chid)

  p[which(p < 0)] <- 0
  log(p)
}
