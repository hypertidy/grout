#' Tile index: pixel offsets and spatial extents for each tile
#'
#' Returns a data frame with one row per tile in a [grout()] scheme,
#' giving the GDAL-style pixel offset (`offset_x`, `offset_y`), the
#' actual pixel dimensions of each tile (`ncol`, `nrow`), and the
#' geographic extent.
#'
#' Tiles along the right or bottom edge may be smaller than the block size
#' when there is a "dangle" (the raster dimensions are not an exact multiple
#' of the block size).
#'
#' Column layout:
#' * `tile` - 1-based tile index (row-major, left-to-right top-to-bottom).
#' * `offset_x`, `offset_y` - 0-based pixel offsets from the top-left corner
#'   of the raster, suitable for passing directly to GDAL `ReadRaster()`.
#' * `tile_col`, `tile_row` - 1-based tile grid coordinates.
#' * `ncol`, `nrow` - pixel dimensions of this tile.
#' * `xmin`, `xmax`, `ymin`, `ymax` - geographic extent of this tile.
#'
#' @param x a `"grout_tiles"` object from [grout()].
#'
#' @return a [tibble::tibble()].
#' @export
#' @importFrom tibble tibble
#' @importFrom vaster col_from_cell row_from_cell cell_from_row cell_from_col
#' @examples
#' g <- grout(c(87, 61), extent = c(0, 1, 0, 1), blocksize = c(32L, 16L))
#' tile_index(g)
#'
#' ## edge case: one tile
#' tile_index(grout(c(61, 87), blocksize = c(61L, 87L)))
tile_index <- function(x) {
  scheme <- x$tileraster
  input  <- x$scheme$inputraster
  ntiles <- prod(scheme$dimension)
  tile   <- seq_len(ntiles)

  offsetX <- (col_from_cell(scheme$dimension, tile) - 1L) * x$scheme$blockX
  offsetY <- (row_from_cell(scheme$dimension, tile) - 1L) * x$scheme$blockY

  nX <- rep(x$scheme$blockX, ntiles)
  nY <- rep(x$scheme$blockY, ntiles)

  ## trim dangle tiles to actual pixel count
  if (x$scheme$dangleX > 0L) {
    right_col <- cell_from_col(scheme$dimension, scheme$dimension[1L])
    nX[right_col] <- x$scheme$blockX - x$scheme$dangleX
  }
  if (x$scheme$dangleY > 0L) {
    bottom_row <- cell_from_row(scheme$dimension, scheme$dimension[2L])
    nY[bottom_row] <- x$scheme$blockY - x$scheme$dangleY
  }

  res  <- diff(input$extent)[c(1L, 3L)] / input$dimension
  xmin <- input$extent[1L] + offsetX * res[1L]
  xmax <- input$extent[1L] + (offsetX + nX) * res[1L]
  ymax <- input$extent[4L] - offsetY * res[2L]
  ymin <- input$extent[4L] - (offsetY + nY) * res[2L]

  tibble::tibble(
    tile     = tile,
    offset_x = offsetX,
    offset_y = offsetY,
    tile_col = (offsetX %/% x$scheme$blockX) + 1L,
    tile_row = (offsetY %/% x$scheme$blockY) + 1L,
    ncol     = nX,
    nrow     = nY,
    xmin     = xmin,
    xmax     = xmax,
    ymin     = ymin,
    ymax     = ymax
  )
}
