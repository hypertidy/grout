#' Tile index
#'
#' Generate a table of tile indexes from a tiling object. 
#' 
#' A data frame with 
#' 
#' * **tile** - index, same as raster cell number
#' * **offset_x** - column offset of tile (index column 0-based)
#' * **offset_y** - row offset of tile (index row 0-based, relative to top row)
#' * **ncol** number of columns in tile (the right and bottom margins may have a dangle based on block size)
#' * **nrow** number of rows in th tile 
#' 
#' Note that ncol,nrow is the block size *unless* the tile is part of a dangling column or row (right or bottom) where
#' the raster doesn't fill complete tiles. 
#' 
#' @param x tiling object, created by [tiles()]
#'
#' @return data frame, see details
#' @export
#'
#' @importFrom tibble tibble
#' @examples
#' tile_index(tiles(raster::raster(volcano), blockX = 32, blockY = 16))
#' ## only one tile in this weird scheme!
#' tile_index(tiles(raster::raster(volcano), blockX = 61, blockY = 87))
tile_index <- function(x) {
  scheme <- x$tileraster
  nc <- raster::ncell(scheme)
  tile <- seq_len(nc)
  offsetX <- (raster::colFromCell(scheme, tile) - 1) * x$scheme$blockX
  offsetY <- (raster::rowFromCell(scheme, tile) - 1)   * x$scheme$blockY
  nX <- rep(x$scheme$blockX, nc)
  nY <- rep(x$scheme$blockY, nc)
  
  if (x$scheme$dangleX > 0) {
    nX[raster::cellFromCol(scheme, ncol(scheme))] <- x$scheme$blockX - x$scheme$dangleX
  }
  if (x$scheme$dangleY > 0) {
    nY[raster::cellFromRow(scheme, nrow(scheme))] <- x$scheme$blockY - x$scheme$dangleY
  }
  tibble::tibble(tile = tile, offset_x = offsetX, offset_y = offsetY, ncol = nX, nrow = nY)
}