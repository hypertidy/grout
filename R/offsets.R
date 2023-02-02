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
#' * xmin, xmax, ymin, ymax - the exent
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
#' @importFrom vaster col_from_cell row_from_cell cell_from_row cell_from_col
#' @examples
#' tile_index(grout(c(87, 61), extent = c(0, 1, 0, 1), blocksize = c(32, 16)))
#' ## only one tile in this weird scheme!
#' tile_index(grout(c(61, 87), blocksize = c(61, 87)))
tile_index <- function(x) {
  scheme <- x$tileraster
  input <- x$scheme$inputraster
  nc <- prod(scheme$dimension)
  tile <- seq_len(nc)
  
  offsetX <- (col_from_cell(scheme$dimension, tile) - 1) * x$scheme$blockX
  offsetY <- (row_from_cell(scheme$dimension, tile) - 1)   * x$scheme$blockY
  nX <- rep(x$scheme$blockX, nc)
  nY <- rep(x$scheme$blockY, nc)
  

    
  if (x$scheme$dangleX > 0) {
    nX[cell_from_col(scheme$dimension, scheme$dimension[1L])] <- x$scheme$blockX - x$scheme$dangleX
  }
  if (x$scheme$dangleY > 0) {
    nY[cell_from_row(scheme$dimension, scheme$dimension[2L])] <- x$scheme$blockY - x$scheme$dangleY
  }
  
    res <- diff(input$extent)[c(1, 3)] / input$dimension
    minX <-   input$extent[1] + offsetX * res[1]
    maxX  <-  input$extent[1] +  (offsetX + nX) * res[1]
    minY <-   input$extent[4] +  (offsetY + nY)     * -res[2]
    maxY <-   input$extent[4] +  offsetY * -res[2]

    
    
  tibble::tibble(tile = tile, offset_x = offsetX, offset_y = offsetY, ncol = nX, nrow = nY, xmin = minX, xmax  = maxX, ymin = minY, ymax = maxY)
}