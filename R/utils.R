raster0 <- function (x, transpose = TRUE) {
#  x <- t(x[, raster::ncol(x):1])
  if (transpose) {
    e <- raster::extent(0, raster::ncol(x), 0, raster::nrow(x))
    
  }
  else {
    e <- raster::extent(0, raster::nrow(x), 0, raster::ncol(x))
    x <- t(x[, raster::ncol(x):1])
    
  }
  raster::raster(raster::setExtent(raster::raster(x), e))
}



