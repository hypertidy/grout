#' Tile index: pixel offsets and spatial extents for each tile
#'
#' Returns a data frame with one row per tile in a [grout()] scheme,
#' giving the GDAL-style pixel offset (`offset_x`, `offset_y`), the
#' actual pixel dimensions of each tile (`ncol`, `nrow`), and the
#' geographic extent.
#'
#' Tiles along the right or bottom edge may be smaller than the block size
#' when there is a "dangle" (the raster dimensions are not an exact multiple
#' of the block size).  By default those tiles are clipped to the pixels that
#' actually exist, which is what a windowed read wants.  With `clip = FALSE`
#' every tile reports the full block size and the edge tiles extend past the
#' grid, which is what a chunk of a tiled format actually occupies on disk -
#' a GDAL block or a Zarr chunk at the margin is a whole block, padded.  The
#' unclipped counts are not recoverable from the clipped index, so this has to
#' be asked for when the index is built.
#'
#' Every column is a function of the tile index and the scheme, so the index
#' does not have to be materialized in full.  Pass `tile` to evaluate only the
#' tiles wanted, in the order wanted; the rows returned are identical to
#' `tile_index(x)[tile, ]` but are computed directly rather than by building
#' and subsetting the whole table.  This matters for schemes with many tiles,
#' where the full index is large but the tiles of interest are few.
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
#' @param tile optional integer vector of 1-based tile indices to return.
#'   The default `NULL` returns every tile in row-major order.  Values may be
#'   given in any order and may repeat; one row is returned per element, in
#'   the order supplied.
#' @param clip clip the edge tiles to the pixels that exist (the default), or
#'   with `FALSE` report every tile at the full block size.
#'
#' @return a [tibble::tibble()].
#' @export
#' @importFrom tibble tibble
#' @importFrom vaster col_from_cell row_from_cell
#' @examples
#' g <- grout(c(87, 61), extent = c(0, 1, 0, 1), blocksize = c(32L, 16L))
#' tile_index(g)
#'
#' ## only the tiles wanted, in the order wanted
#' tile_index(g, tile = c(4, 1, 9))
#'
#' ## full blocks, including the dangle past the grid edge
#' tile_index(g, clip = FALSE)
#'
#' ## edge case: one tile
#' tile_index(grout(c(61, 87), blocksize = c(61L, 87L)))
tile_index <- function(x, tile = NULL, clip = TRUE) {
  tiledim <- x$tileraster$dimension
  input   <- x$scheme$inputraster
  blockX  <- x$scheme$blockX
  blockY  <- x$scheme$blockY
  ntiles  <- as.integer(prod(tiledim))

  if (is.null(tile)) {
    tile <- seq_len(ntiles)
  } else {
    tile <- as.integer(tile)
    if (anyNA(tile) || any(tile < 1L) || any(tile > ntiles)) {
      stop(sprintf("'tile' must be between 1 and %i (the number of tiles)",
                   ntiles), call. = FALSE)
    }
  }

  tileCol <- col_from_cell(tiledim, tile)
  tileRow <- row_from_cell(tiledim, tile)

  offsetX <- (tileCol - 1L) * blockX
  offsetY <- (tileRow - 1L) * blockY

  nX <- rep(blockX, length(tile))
  nY <- rep(blockY, length(tile))

  ## trim dangle tiles to actual pixel count (the last tile column/row)
  if (clip) {
    if (x$scheme$dangleX > 0L) {
      nX[tileCol == tiledim[1L]] <- blockX - x$scheme$dangleX
    }
    if (x$scheme$dangleY > 0L) {
      nY[tileRow == tiledim[2L]] <- blockY - x$scheme$dangleY
    }
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
    tile_col = tileCol,
    tile_row = tileRow,
    ncol     = nX,
    nrow     = nY,
    xmin     = xmin,
    xmax     = xmax,
    ymin     = ymin,
    ymax     = ymax
  )
}
