# .halfactors <- function(n) {
#  2 ^ (seq(trunc(log(n%/%2, 2))))
# 
# }
# 
# .bestfactors <- function(n, minblock = 4) {
#   ss <- .halfactors(n)
#   sq <- seq(n)
#   sq <- sq[(n / (sq )) == (n %/% (sq))]
#   list(ss, sq)
# }
# 
# bf <- .bestfactors(5400)
# ## from gris
# 
# .edgesXY <- function(x) {
#  as.matrix(expand.grid(seq(xmin(x), xmax(x), length = ncol(x) + 1),
#                         seq(ymax(x), ymin(x), length = nrow(x) + 1)
#   ))
# }
# 
