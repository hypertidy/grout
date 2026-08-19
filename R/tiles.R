
## Internal: compute tile scheme from a grid spec list and blocksize
## grid spec: list(dimension = c(ncol, nrow), extent = c(xmin, xmax, ymin, ymax), projection = <chr>)
.tilescheme <- function(x, blocksize) {
  dm <- as.integer(x$dimension)
  blockX <- as.integer(blocksize[1L])
  blockY <- as.integer(blocksize[2L])

  ntilesX <- max(1L, ceiling(dm[1L] / blockX))
  ntilesY <- max(1L, ceiling(dm[2L] / blockY))

  ## pixels "dangling" past the last full block (overhang)
  dangleX <- (ntilesX * blockX) - dm[1L]
  dangleY <- (ntilesY * blockY) - dm[2L]

  structure(
    list(
      inputraster = x,
      ntilesX = ntilesX, ntilesY = ntilesY,
      dangleX = dangleX, dangleY = dangleY,
      blockX  = blockX,  blockY  = blockY
    ),
    class = "grout_tilescheme"
  )
}

## Extent of the tiled grid (may extend beyond the input if there's dangle)
.tilescheme_extent <- function(ts) {
  ex  <- ts$inputraster$extent
  res <- diff(ex)[c(1L, 3L)] / ts$inputraster$dimension
  c(
    ex[1L],
    ex[1L] + ts$ntilesX * ts$blockX * res[1L],
    ex[4L] - ts$ntilesY * ts$blockY * res[2L],
    ex[4L]
  )
}

#' Create a tiling scheme from a raster grid specification
#'
#' Given a grid (dimension + extent), compute how it maps onto tiles of a
#' given block size. The result records the tile count in each dimension and
#' the "dangle" - the number of extra pixels that arise when the grid
#' dimensions do not divide evenly into the block size.
#'
#' Use [tile_index()] to turn the scheme into a data frame of pixel offsets
#' and spatial extents, one row per tile.
#'
#' @param dimension integer vector `c(ncol, nrow)` of the raster grid.
#' @param extent numeric vector `c(xmin, xmax, ymin, ymax)`.  Defaults to
#'   `c(0, ncol, 0, nrow)` (pixel-coordinate space).
#' @param blocksize integer vector `c(block_ncol, block_nrow)`.  Defaults to
#'   `c(256L, 256L)`.
#' @param projection CRS string (e.g. `"EPSG:4326"`).  Stored but not used
#'   computationally.
#'
#' @return A `"grout_tiles"` object: a list with
#'   * `$tileraster` - grid spec (`dimension`, `extent`, `projection`) of the
#'     *tile* grid (one cell per tile).
#'   * `$scheme` - internal `"grout_tilescheme"` with block sizes, tile counts,
#'     and dangle values.
#'
#' @seealso [tile_index()], [tile_spec()]
#' @export
#' @examples
#' ## clean fit
#' grout(c(16, 12), extent = c(0, 16, 0, 12), blocksize = c(4L, 4L))
#'
#' ## dangle in both dimensions
#' grout(c(15, 13), extent = c(0, 15, 0, 13), blocksize = c(4L, 4L))
#'
#' ## default pixel-coordinate extent
#' grout(c(87, 61), blocksize = c(12L, 16L))
grout <- function(dimension, extent = NULL, blocksize = c(256L, 256L),
                  projection = NA_character_) {
  dimension <- as.integer(dimension)
  stopifnot(length(dimension) == 2L, all(dimension > 0L))

  if (is.null(extent)) extent <- c(0, dimension[1L], 0, dimension[2L])
  stopifnot(length(extent) == 4L)

  blocksize <- as.integer(blocksize)
  stopifnot(length(blocksize) == 2L, all(blocksize > 0L))

  grid <- list(dimension = dimension, extent = extent, projection = projection)
  ts   <- .tilescheme(grid, blocksize)
  rt   <- list(
    dimension  = c(ts$ntilesX, ts$ntilesY),
    extent     = .tilescheme_extent(ts),
    projection = projection
  )

  structure(
    list(tileraster = rt, scheme = ts),
    class = "grout_tiles"
  )
}

#' @importFrom vaster x_res y_res
#' @export
print.grout_tiles <- function(x, ...) {
  dm <- x$tileraster$dimension
  ex <- x$tileraster$extent
  rx <- vaster::x_res(dm, ex)
  ry <- vaster::y_res(dm, ex)
  cat(sprintf("          tiles: %i x %i  (%i total)\n",
              dm[1L], dm[2L], prod(dm)))
  cat(sprintf("          block: %i x %i\n",
              x$scheme$blockX, x$scheme$blockY))
  cat(sprintf("         dangle: %i, %i  (col, row)\n",
              x$scheme$dangleX, x$scheme$dangleY))
  cat(sprintf("tile resolution: %s x %s\n", format(rx), format(ry)))
  cat(sprintf("    tile extent: %s, %s, %s, %s  (xmin,xmax,ymin,ymax)\n",
              format(ex[1L]), format(ex[2L]),
              format(ex[3L]), format(ex[4L])))
  invisible(x)
}

#' Plot a tiling scheme
#'
#' Draws each tile as a rectangle (grey border) with the original raster
#' extent overlaid as a dashed red outline.
#'
#' @param x a `"grout_tiles"` object from [grout()].
#' @param ... passed to [vaster::plot_extent()].
#' @param add add to the current plot?  Default `FALSE`.
#' @param border colour for the tile borders.  Default `"grey"`.
#' @param lwd line width for tile borders.  Default `2`.
#' @importFrom graphics plot
#' @returns the input 'x', invisibly
#' @export
#' @examples
#' g <- grout(c(44, 30), blocksize = c(12L, 12L))
#' plot(g)
#'
#' ## overlay a second scheme
#' g2 <- grout(c(44, 30), blocksize = c(8L, 8L))
#' plot(g2, add = TRUE, border = "steelblue")
plot.grout_tiles <- function(x, ..., add = FALSE, border = "grey", lwd = 2) {
  rects <- as_rect_grout(x)
  ex0 <- unlist(rects[1L, c("xmin","xmax","ymin","ymax")])
  vaster::plot_extent(rects, add = add, border = border, lwd = lwd)
  # for (i in seq_len(nrow(rects))) {
  #   vaster::plot_extent(unlist(rects[i, c("xmin","xmax","ymin","ymax")]),
  #                      add = TRUE, border = border, lwd = lwd, ...)
  # }
  vaster::plot_extent(x$scheme$inputraster$extent,
                     add = TRUE, lty = 2L, border = "firebrick")
  invisible(x)
}

## Internal: expand tile grid corners to a (xmin,xmax,ymin,ymax) tibble.
## One row per tile, in row-major order (left-to-right, top-to-bottom).
#' @importFrom utils head tail
as_rect_grout <- function(x) {
  xs <- vaster::x_corner(x$tileraster$dimension, x$tileraster$extent)
  ys <- vaster::y_corner(x$tileraster$dimension, x$tileraster$extent)
  grd <- cbind(
    expand.grid(xmin = head(xs, -1L), ymin = head(ys, -1L)),
    expand.grid(xmax = tail(xs, -1L), ymax = tail(ys, -1L))
  )[, c("xmin", "xmax", "ymin", "ymax")]
  tibble::as_tibble(grd)

}
