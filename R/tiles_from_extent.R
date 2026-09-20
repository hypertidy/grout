#' Which tiles does an extent intersect?
#'
#' Solve for the tile indices a region overlaps, without building the tile
#' index.  The answer costs a few arithmetic operations regardless of how many
#' tiles the scheme has, because it is computed from the tile grid rather than
#' found by testing every tile.
#'
#' The result is intended to be handed straight to [tile_index()]:
#'
#' ```
#' tile_index(x, tile = tiles_from_extent(x, region))
#' ```
#'
#' Tiles are treated as half open, so a region that only touches a tile
#' boundary does not pick up the tile on the far side of it: a tile owns its
#' left and top edges and not its right and bottom ones.  A region with zero
#' width or height (a point) still lands on one tile.  Regions are clamped to
#' the scheme, so a region larger than the grid returns every tile, and a
#' region entirely outside it returns `integer(0)`.
#'
#' Note that the tile grid can extend past the grid it tiles, when there is a
#' dangle, so tiles can be returned for a region that lies outside the extent
#' passed to [grout()] but inside the tiled area.
#'
#' @param x a `"grout_tiles"` object from [grout()].
#' @param extent numeric vector `c(xmin, xmax, ymin, ymax)`, in the coordinate
#'   system of the scheme.
#' @param tol snapping tolerance, in tile widths.  A region edge within `tol`
#'   of a tile boundary is treated as being on it, which keeps the answer
#'   stable when an extent has been arrived at by arithmetic rather than
#'   given exactly.
#'
#' @return integer vector of 1-based tile indices, in increasing (row-major)
#'   order, or `integer(0)` when the region does not overlap the scheme.
#' @export
#' @importFrom vaster x_res y_res cell_from_row_col
#' @seealso [tile_index()]
#' @examples
#' g <- grout(c(87, 61), blocksize = c(12L, 16L))
#'
#' tiles_from_extent(g, c(20, 40, 20, 40))
#'
#' ## straight into tile_index()
#' tile_index(g, tile = tiles_from_extent(g, c(20, 40, 20, 40)))
#'
#' ## a whole-grid region is every tile; a region outside is none
#' length(tiles_from_extent(g, c(0, 87, 0, 61)))
#' tiles_from_extent(g, c(1000, 2000, 1000, 2000))
tiles_from_extent <- function(x, extent, tol = 1e-8) {
  tiledim <- x$tileraster$dimension
  tileext <- x$tileraster$extent

  extent <- as.numeric(extent)
  stopifnot(length(extent) == 4L)
  if (anyNA(extent)) stop("'extent' must not contain NA", call. = FALSE)
  if (extent[1L] > extent[2L] || extent[3L] > extent[4L]) {
    stop("'extent' must be c(xmin, xmax, ymin, ymax) with xmin <= xmax and ymin <= ymax",
         call. = FALSE)
  }

  xres <- x_res(tiledim, tileext)
  yres <- y_res(tiledim, tileext)

  ## continuous position in tile units, snapped, then half open
  col0 <- floor((extent[1L] - tileext[1L]) / xres + tol) + 1
  col1 <- ceiling((extent[2L] - tileext[1L]) / xres - tol)
  row0 <- floor((tileext[4L] - extent[4L]) / yres + tol) + 1
  row1 <- ceiling((tileext[4L] - extent[3L]) / yres - tol)

  ## a zero width or height region still lands on one tile
  col1 <- max(col1, col0)
  row1 <- max(row1, row0)

  ## clamp to the scheme
  col0 <- max(col0, 1); col1 <- min(col1, tiledim[1L])
  row0 <- max(row0, 1); row1 <- min(row1, tiledim[2L])
  if (col0 > col1 || row0 > row1) return(integer(0))

  rc <- expand.grid(col = seq.int(col0, col1), row = seq.int(row0, row1))
  sort(as.integer(cell_from_row_col(tiledim, rc$row, rc$col)))
}
