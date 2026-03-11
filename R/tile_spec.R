#' Tile specification for a standard tiling profile
#'
#' Given a raster grid and a zoom level, find all tiles that intersect the
#' grid extent within a standard global tiling profile (Mercator, geodetic,
#' or native raster).
#'
#' Profiles:
#' * `"mercator"` - Web Mercator (EPSG:3857), global extent
#'   +/-20037508.34 m.
#' * `"geodetic"` - geographic coordinates (EPSG:4326), global extent
#'   +/-180, +/-90.
#' * `"raster"` - uses the input `extent` as the full domain.
#'
#' `tile_row` in the output uses TMS orientation (row 0 at the bottom) by
#' default.  Set `xyz = TRUE` for XYZ/slippy-map orientation (row 0 at the
#' top).
#'
#' @param dimension integer `c(ncol, nrow)` of the source raster.
#' @param extent numeric `c(xmin, xmax, ymin, ymax)` of the source raster.
#'   Must be in the coordinate system of the chosen `profile`.
#' @param zoom integer zoom level (0+).
#' @param blocksize tile size in pixels, default `c(256L, 256L)`.
#' @param profile one of `"mercator"`, `"geodetic"`, `"raster"`.
#' @param crs CRS string, only used when `profile = "raster"`.
#' @param xyz if `TRUE`, use XYZ tile row convention (0 at top).
#'
#' @return a data frame with columns `tile`, `tile_col`, `tile_row`, `zoom`,
#'   `xmin`, `xmax`, `ymin`, `ymax`, `ncol`, `nrow`, `crs`.
#' @export
#' @importFrom vaster col_from_x row_from_y cell_from_row_col vcrop
#' @seealso [tile_zoom()] to determine the appropriate zoom level.
#' @examples
#' tile_spec(c(8194, 8194), c(140, 155, -45, -30), profile = "geodetic")
#'
#' tile_spec(c(2048, 248), c(140, 155, -45, -30), zoom = 5,
#'           profile = "geodetic", blocksize = c(512L, 512L))
tile_spec <- function(dimension, extent, zoom = 0L,
                      blocksize = c(256L, 256L),
                      profile = c("mercator", "geodetic", "raster"),
                      crs = NA_character_,
                      xyz = FALSE) {
  profile   <- match.arg(profile)
  blocksize <- as.integer(blocksize)
  zoom      <- as.integer(zoom)

  if (profile == "mercator" && any(diff(extent)[c(1L, 3L)] < 360)) {
    message("very small region for Mercator - is this a geodetic extent? ",
            "(use profile = \"geodetic\")")
  }
  if (profile != "raster" && !is.na(crs)) {
    message("'crs' is ignored for mercator and geodetic profiles")
  }

  global <- switch(profile,
    mercator = c(-1, 1, -1, 1) * 20037508.342789244,
    geodetic = c(-180, 180, -90, 90),
    raster   = extent
  )
  crs <- switch(profile,
    mercator = "EPSG:3857",
    geodetic = "EPSG:4326",
    raster   = crs
  )

  ## affine transform at zoom 0 (one tile = full globe)
  transform <- c(global[1L],  diff(global[1:2]) / blocksize[1L], 0,
                 global[4L],  0, -diff(global[3:4]) / blocksize[2L])
  transform[c(2L, 6L)] <- transform[c(2L, 6L)] / 2^zoom
  idimension <- blocksize * 2L^zoom

  v   <- vcrop(extent, idimension, global, snap = "out")
  col <- col_from_x(idimension, global, v$extent[1:2]) %/% blocksize[1L]
  row <- row_from_y(idimension, global, v$extent[3:4]) %/% blocksize[2L]
  maxcolrow <- idimension / blocksize

  xs <- seq(col[1L], col[2L])
  ys <- seq(row[1L], row[2L])
  if (maxcolrow[1L] > 0L) xs <- setdiff(xs, maxcolrow[1L])
  if (maxcolrow[2L] > 0L) ys <- setdiff(ys, maxcolrow[2L])

  ys    <- rep(ys, each = length(xs))
  index <- cell_from_row_col(idimension %/% blocksize, ys + 1L, xs + 1L) - 1L

  tilesize <- abs(transform[c(2L, 6L)]) * blocksize

  tt <- data.frame(tile = index, tile_col = xs, tile_row = ys)
  tt$zoom <- zoom
  tt$xmin <- transform[1L] +  tt$tile_col      * tilesize[1L]
  tt$xmax <- transform[1L] + (tt$tile_col + 1L) * tilesize[1L]
  tt$ymin <- transform[4L] - (tt$tile_row + 1L) * tilesize[2L]
  tt$ymax <- transform[4L] -  tt$tile_row       * tilesize[2L]

  if (!xyz) {
    ## convert from XYZ to TMS (flip row so 0 is at the bottom)
    tt$tile_row <- (idimension[2L] %/% blocksize[2L]) - tt$tile_row - 1L
  }

  tt$ncol <- blocksize[1L]
  tt$nrow <- blocksize[2L]
  tt$crs  <- crs
  tt
}

#' Natural maximum zoom for a raster grid
#'
#' Returns the largest zoom level at which the tile resolution is still
#' coarser than (or equal to) the native raster resolution.  This is the
#' zoom at which the data can be served without upsampling.
#'
#' @inheritParams tile_spec
#' @return integer zoom level.
#' @export
#' @name tile_zoom
#' @examples
#' tile_zoom(c(8194, 8194), c(140, 155, -45, -30), profile = "geodetic")
tile_zoom <- function(dimension, extent,
                      blocksize = c(256L, 256L),
                      profile   = c("mercator", "geodetic", "raster")) {
  profile   <- match.arg(profile)
  blocksize <- as.integer(blocksize)

  if (profile == "mercator" && any(diff(extent)[c(1L, 3L)] < 360)) {
    message("very small region for Mercator - is this a geodetic extent? ",
            "(use profile = \"geodetic\")")
  }

  global <- switch(profile,
    mercator = c(-1, 1, -1, 1) * 20037508.342789244,
    geodetic = c(-180, 180, -90, 90),
    raster   = extent
  )

  transform      <- c(global[1L],  diff(global[1:2]) / blocksize[1L], 0,
                      global[4L],  0, -diff(global[3:4]) / blocksize[2L])
  nativeresolution <- diff(extent)[c(1L, 3L)] / dimension

  for (zoom in 0:23) {
    if (all(abs(transform[c(2L, 6L)]) <= nativeresolution)) return(zoom)
    transform[c(2L, 6L)] <- transform[c(2L, 6L)] / 2
  }
  zoom
}
