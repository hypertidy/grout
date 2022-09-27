
# from a raster or matrix, tile out
# the dimensions based on blockX/blockY
.tilescheme <- function(x, blocksize) {
  dm <- as.integer(x$dimension)
  blockX <- blocksize[1L]
  blockY <- blocksize[2L]
  if (dm[1L] <= blockX) {
    ntilesX <- 1L
  } else {
    ntilesX <- (dm[1L] %/% blockX)
    if (ntilesX * blockX < dm[1L]) ntilesX <- ntilesX + 1L  ## hack
  }
  if (blockX == 1) ntilesX <- dm[1L]
  dangleX <- (ntilesX * blockX) - dm[1L] 
  if (dm[2L] <= blockY) {
    ntilesY <- 1L
  } else {
    ntilesY <- (dm[2L] %/% blockY)
  }
  if (ntilesY * blockY < dm[2L]) ntilesY <- ntilesY + 1  ## hack
  if (blockY == 1) ntilesY <- dm[2L]
  dangleY <- (ntilesY * blockY) - dm[2L] 
  structure(list(inputraster = x, 
       ntilesX = ntilesX, ntilesY = ntilesY, 
       dangleX = dangleX, dangleY = dangleY, 
       blockX = blockX, blockY = blockY), 
       class = "grout_tilescheme")

}
#' @importFrom vaster x_res y_res
extent.grout_tilescheme <- function(x) {
                c(x$inputraster$extent[1L], 
                 x$inputraster$extent[1L] +  (x$blockX *  x$ntilesX) * vaster:::x_res(x$inputraster$dimension, 
                                                                                      x$inputraster$extent), 
                 x$inputraster$extent[4L] -  (x$blockY *  x$ntilesY) * vaster:::y_res(x$inputraster$dimension, 
                                                                                      x$inputraster$extent), 
                 x$inputraster$extent[4L] )
}

#' Create a tiling scheme from a raster
#'
#' The input may be an actual raster or a matrix. There is an assumed
#' block size of 256x256, and the scheme will record the number of tiles
#' in each dimension and the amount of "overlapping dangle" when the dimensions
#' of the data don't fit neatly within the tiles. 
#' 
#' The tile scheme object has print and plot methods for basic information. 
#' 
#' See function [as_polys()] to generate tiles from the scheme. 
#' @param x a raster specification, a list with extent, dimension, projection
#' @param blockX tile dimensions in columns (X)
#' @param blockY tile diemnsions in rows (Y)
#'
#' @return A "tile scheme" object with information about the tile spacing and extent. 
#' @export
#'
#' @examples

#' ## one (too) big tile
#' tile <- grout(volcano, c(256L, 256L))
#' plot(tile)
#' tile
#' 
#' raster::image(rv, add = TRUE)
#' axis(1); axis(2)
#' 
#' ## more size appropriate
#' tils <- grout(volcano, c(8, 8))
#' plot(tils)
#' raster::image(rv, add = TRUE)
#' plot(tils, add = TRUE)
grout <- function(x, blocksize = NULL) {
  if (is.array(x)) {
    x <- list(extent = c(0, nrow(x), 0, ncol(x)), 
              dimension = dim(x)[1:2], 
              projection = NA_character_)
  }
  has_names <- function(x, nms) {
    all(nms %in% names(x))
  }
  edp <- c("extent", "dimension", "projection")
  if (is.character(x)) {
    x <- vapour::vapour_raster_info(x)
  }
  if (is.list(x) && has_names(x, edp)) {
    if (is.null(blocksize) && has_names(x, "tiles")) {
        blocksize <- x$tiles
    }
  } else {
    if (is.list(x) && has_names(x, c("extent", "dimXY", "projection"))) {
      x <- setNames(x[c("extent", "dimXY", "projection")], edp)
    }
      
  }
  if (is.null(blocksize)) blocksize <- c(x$dimension[1L], 1L)
  ts <- .tilescheme(x, blocksize)

   # rt <- raster::raster(extent.grout_tilescheme(ts), 
   #                     ncol = ts$ntilesX, nrow = ts$ntilesY, crs = raster::crs(x))
  rt <-  list(extent = extent.grout_tilescheme(ts), dimension = c(ts$ntilesX, ts$ntilesY), projection = x$projection)
  structure(list(tileraster = rt,
                 scheme = ts),
            class = "grout_tiles")
}

tiles <- function(... )  {
  .Deprecated("grout")
  grout(...)
}
#' print tiles
#' @param x a grout [tiles()] object
#' @param ... ignored
#' @export
print.grout_tiles <- function(x, ...) { 
  dm <- x$tileraster$dimension
  ex <- x$tileraster$extent
  cat(sprintf("          tiles: %i, %i (x * y = %i)\n",  dm[2L], dm[1L], prod(dm)) )
  cat(sprintf("          block: %i, %i \n", x$scheme$blockX, x$scheme$blockY) )
  cat(sprintf("         dangle: %i, %i \n", x$scheme$dangleX, x$scheme$dangleY))
  rs <- c(vaster:::x_res(dm, ex), vaster:::y_res(dm, ex))
  cat(sprintf("tile resolution: %s, %s \n", format(rs[1L]), format(rs[2L])))
  cat(sprintf("    tile extent: %s, %s, %s, %s (xmin,xmax,ymin,ymax)\n", 
              format(ex[1L]), 
                     format(ex[2L]), 
                            format(ex[3L]),
                                   format(ex[4L])))
 
  rg <- c(vaster:::x_res(dm, ex), 
          vaster:::y_res(dm, ex))
  cat(sprintf("          grain: %s, %s (%i : x, %i : y)", format(rg[1L]), format(rg[2L]), x$scheme$blockX, x$scheme$blockY))
  invisible(NULL)
}

#' plot tiles
#' 
#' @param x a grout [tiles()] object
#' @param ... arguments passed to methods 
#' @param border the colour of the tile border (default grey)
#' @param lwd the width of the tile border (default = 2)
#' @importFrom graphics plot
#' @export
plot.grout_tiles <- function(x, ..., border = "grey", lwd = 2) {
  stop("not working atm")
  # sp::plot(as_polys(x), ..., border = border, lwd = lwd)
  # raster::plot(raster::extent(x$scheme$inputraster), add = TRUE, col = "firebrick", lty = 2)
  # invisible(NULL)
}

#' Create rect (extent) table
#' 
#' Data frame of extents xmin, xmax, ymin, ymax. 
#' 
#' @param x  a grout [tiles()] object 
#'
#' @param ... ignored
#'
#' @name as_rect
#' @return data frame of tile extents
#' @export
#' 
as_rect <- function(x, ...) {
  UseMethod("as_rect")
}
.p2s <- function(x) {
  cbind(vx0 = x[-length(x)],
  .vx1 = x[-1L])
} 
#' @name as_rect
#' @export
as_rect.grout_tiles <- function(x, ...) {
  stop("not working atm")
  # xx <- seq(raster::xmin(x$tileraster), raster::xmax(x$tileraster), length = ncol(x$tileraster) + 1L)
  # yy <- seq(raster::ymin(x$tileraster), raster::ymax(x$tileraster), length = nrow(x$tileraster) + 1L)
  # 
  # idx <- .p2s(seq_along(xx))
  # idy <- .p2s(seq_along(yy))
  # tibble::as_tibble(cbind(expand.grid(x0 = xx[idx[,1L]], y0 = yy[idy[,1L]]), 
  #                expand.grid(x1 = xx[idx[,2L]], y1 = yy[idy[,2L]])))
}
#' Tiles as polygons
#' 
#' @param x a grout [tiles()] object 
#' @param ... ignored
#'
#' @export
as_polys <- function(x, ...) {
  UseMethod("as_polys")
}


#' @name as_polys
#' @importFrom methods as
#' @export
as_polys.grout_tiles <- function(x, ...) {
   stop("not working atm")
  ## WIP
  xs <- x_corner(x$tileraster)
  ys <- y_corner(x$tileraster)
  cell <- seq_len(prod(x$dimension))
  idx <- col_from_cell(x$tileraster, cell)
  jdx <- row_from_cell(x$tileraster, cell)
  
  grd <- expand.grid(idx, jdx)
  grd <- cbind(grd, grd + 1)
  # pp <- as(x$tileraster, "SpatialPolygonsDataFrame")
  # pp$tile <- seq(raster::ncell(x$tileraster))
  # pp$layer <- NULL
  pp
}