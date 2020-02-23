
# from a raster or matrix, tile out
# the dimensions based on blockX/blockY
.tilescheme <- function(x, blockX = 256L, blockY = 256L) {
  dm <- as.integer(dim(x))[1:2]
  blockX <- as.integer(blockX)
  blockY <- as.integer(blockY)
  if (dm[2L] <= blockX) {
    ntilesX <- 1L
  } else {
    ntilesX <- (dm[2L] %/% blockX)
    if (ntilesX * blockX < dm[2L]) ntilesX <- ntilesX + 1L  ## hack
  }
  if (blockX == 1) ntilesX <- dm[2L]
  dangleX <- (ntilesX * blockX) - dm[2L] 
  if (dm[1L] <= blockY) {
    ntilesY <- 1L
  } else {
    ntilesY <- (dm[1L] %/% blockY)
  }
  if (ntilesY * blockY < dm[1L]) ntilesY <- ntilesY + 1  ## hack
  if (blockY == 1) ntilesY <- dm[1L]
  dangleY <- (ntilesY * blockY) - dm[1L] 
  structure(list(inputraster = raster::raster(x), 
       ntilesX = ntilesX, ntilesY = ntilesY, 
       dangleX = dangleX, dangleY = dangleY, 
       blockX = blockX, blockY = blockY), 
       class = "grout_tilescheme")

}

extent.grout_tilescheme <- function(x) {
  raster::extent(raster::xmin(x$inputraster), 
                 raster::xmin(x$inputraster) +  (x$blockX *  x$ntilesX) * raster::res(x$inputraster)[1], 
                 raster::ymax(x$inputraster) -  (x$blockY *  x$ntilesY) * raster::res(x$inputraster)[2], 
                 raster::ymax(x$inputraster) )
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
#' @param x a raster, or a matrix
#' @param blockX tile dimensions in columns (X)
#' @param blockY tile diemnsions in rows (Y)
#'
#' @return A "tile scheme" object with information about the tile spacing and extent. 
#' @export
#'
#' @examples
#' rv <- raster::raster(volcano, xmn = 0, xmx = nrow(volcano), 
#'                               ymn = 0, ymx = ncol(volcano))
#' ## one (too) big tile
#' tile <- tiles(rv)
#' plot(tile)
#' tile
#' 
#' raster::image(rv, add = TRUE)
#' axis(1); axis(2)
#' 
#' ## more size appropriate
#' tils <- tiles(rv, 8, 8)
#' plot(tils)
#' raster::image(rv, add = TRUE)
#' plot(tils, add = TRUE)
tiles <- function(x, blockX = 256, blockY = 256) {
  ts <- .tilescheme(x, blockX = blockX, blockY = blockY)
  rt <- raster::raster(extent.grout_tilescheme(ts), 
                       ncol = ts$ntilesX, nrow = ts$ntilesY)
  structure(list(tileraster = rt,
                 scheme = ts),
            class = "grout_tiles")
}

#' print tiles
#' @param x a grout [tiles()] object
#' @param ... ignored
#' @export
print.grout_tiles <- function(x, ...) { 
  dm <- dim(x$tileraster)
  ex <- raster::extent(x$tileraster)
  cat(sprintf("          tiles: %i, %i (x * y = %i)\n",  dm[2L], dm[1L], prod(dm)) )
  cat(sprintf("          block: %i, %i \n", x$scheme$blockX, x$scheme$blockY) )
  cat(sprintf("         dangle: %i, %i \n", x$scheme$dangleX, x$scheme$dangleY))
  rs <- raster::res(x$tileraster)
  cat(sprintf("tile resolution: %s, %s \n", format(rs[1L]), format(rs[2L])))
  cat(sprintf("    tile extent: %s, %s, %s, %s (xmin,xmax,ymin,ymax)\n", 
              format(raster::xmin(ex)), 
                     format(raster::xmax(ex)), 
                            format(raster::ymin(ex)),
                                   format(raster::ymax(ex))))
  rg <- raster::res(x$scheme$inputraster)
  cat(sprintf("          grain: %s, %s (%i : x, %i : y)", format(rg[1L]), format(rg[2L]), x$scheme$blockX, x$scheme$blockY))
  invisible(NULL)
}

#' plot tiles
#' 
#' @param x a grout [tiles()] object
#' @param ... arguments passed to methods (particularly [sp::plot()])
#' @param border the colour of the tile border (default grey)
#' @param lwd the width of the tile border (default = 2)
#' @importFrom graphics plot
#' @export
plot.grout_tiles <- function(x, ..., border = "grey", lwd = 2) {
  sp::plot(as_polys(x), ..., border = border, lwd = lwd)
  raster::plot(raster::extent(x$scheme$inputraster), add = TRUE, col = "firebrick", lty = 2)
  invisible(NULL)
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
  pp <- as(x$tileraster, "SpatialPolygonsDataFrame")
  pp$tile <- seq(raster::ncell(x$tileraster))
  pp$layer <- NULL
  pp
}