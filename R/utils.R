raster0 <- function (x, transpose = TRUE) {
  x <- t(x[, raster::ncol(x):1])
  if (transpose) {
    e <- raster::extent(0, raster::ncol(x), 0, raster::nrow(x))
  }
  else {
    e <- raster::extent(0, raster::nrow(x), 0, raster::ncol(x))
  }
  raster::setExtent(raster::raster(x), e)
}


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

