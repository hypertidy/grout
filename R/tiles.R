
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
                 x$inputraster$extent[1L] +  (x$blockX *  x$ntilesX) * vaster::x_res(x$inputraster$dimension, 
                                                                                      x$inputraster$extent), 
                 x$inputraster$extent[4L] -  (x$blockY *  x$ntilesY) * vaster::y_res(x$inputraster$dimension, 
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
#' If extent is not provided the default 'xmin=0,xmax=ncol,ymin=0,ymax=nrow' is used. 
#' 
#' The tile scheme object has print and plot methods for basic information. 
#' 
#' See example in the README with 'wk::rct' to generate plot-able and efficient 
#' spatial objects from the scheme. 
#' 
#' @param dimension number of columns and rows of the raster grid
#' @param extent extent of the raster grid xmin,xmax,ymin,ymax
#' @param blocksize tile dimensions in columns (X) and rows (Y)
#' @param projection the projection (crs) of the grid
#' 
#' @return A "tile scheme" object with information about the tile spacing and extent. 
#' @export
#'
#' @examples
#' ## one block tile (too big)
#' grout(c(87, 61), blocksize = c(256L, 256L))
#' ## more size appropriate
#' grout(c(87, 61), blocksize = c(8, 8))
#' grout(c(10, 20), c(0, 1, 0, 2), blocksize = c(256, 256))
grout <- function(dimension, extent = NULL, blocksize = NULL, projection = NA_character_) {
  if (is.null(extent)) extent <- c(0, dimension[1L], 0, dimension[2L])
  x <- list(extent = extent, dimension = dimension, projection = projection)
  
  .groutfrom(x,  blocksize)
}
.groutfrom <- function(x, blocksize = NULL) {
  ## disallow array

  has_names <- function(x, nms) {
    all(nms %in% names(x))
  }
  edp <- c("extent", "dimension", "projection")

  if (is.list(x) && has_names(x, edp)) {
    if (is.null(blocksize) && has_names(x, "tiles")) {
        blocksize <- x$tiles
    }
  } else {
    if (is.list(x) && has_names(x, c("extent", "dimXY", "projection"))) {
      x <- stats::setNames(x[c("extent", "dimXY", "projection")], edp)
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


#' print tiles
#' @param x a grout [tiles()] object
#' @param ... ignored
#' @importFrom vaster x_res y_res
#' @export
print.grout_tiles <- function(x, ...) { 
  dm <- x$tileraster$dimension
  ex <- x$tileraster$extent
  cat(sprintf("          tiles: %i, %i (x * y = %i)\n",  dm[2L], dm[1L], prod(dm)) )
  cat(sprintf("          block: %i, %i \n", x$scheme$blockX, x$scheme$blockY) )
  cat(sprintf("         dangle: %i, %i \n", x$scheme$dangleX, x$scheme$dangleY))
  rs <- c(vaster::x_res(dm, ex), vaster::y_res(dm, ex))
  cat(sprintf("tile resolution: %s, %s \n", format(rs[1L]), format(rs[2L])))
  cat(sprintf("    tile extent: %s, %s, %s, %s (xmin,xmax,ymin,ymax)\n", 
              format(ex[1L]), 
                     format(ex[2L]), 
                            format(ex[3L]),
                                   format(ex[4L])))
 
  rg <- c(vaster::x_res(dm, ex), 
          vaster::y_res(dm, ex))
  cat(sprintf("          grain: %s, %s (%i : x, %i : y)", format(rg[1L]), format(rg[2L]), x$scheme$blockX, x$scheme$blockY))
  invisible(NULL)
}

#' plot tiles
#' 
#' @param x a grout [tiles()] object
#' @param ... arguments passed to methods 
#' @param add add to current plot or start a new one (default is `FALSE`, a new plot)
#' @param border the colour of the tile border (default grey)
#' @param lwd the width of the tile border (default = 2)
#' @importFrom graphics plot
#' @export
plot.grout_tiles <- function(x, ..., add = FALSE, border = "grey", lwd = 2) {
  ext <- as_rect.grout_tiles(x)
  vaster::plot_extent(ext, add = add, border = border, lwd = lwd, ...)
  vaster::plot_extent(x$scheme$inputraster$extent, add = TRUE, lty = 2, border = "firebrick")
  invisible(NULL)
}


.p2s <- function(x) {
  cbind(vx0 = x[-length(x)],
  .vx1 = x[-1L])
} 

#' @importFrom utils head tail
as_rect.grout_tiles <- function(x, ...) {
   #stop("not working atm")
  ## WIP
  xs <- vaster::x_corner(x$tileraster$dimension, 
                         x$tileraster$extent)
  ys <- vaster::y_corner(x$tileraster$dimension, 
                 x$tileraster$extent)
  # cell <- seq_len(prod(x$tileraster$dimension))
  # idx <- vaster::col_from_cell(x$tileraster$dimension, cell)
  # jdx <- vaster::row_from_cell(x$tileraster$dimension, cell)
  # 
  # grd <- expand.grid(idx, jdx)
  # grd <- cbind(grd, grd + 1)
  # 
  grd <- cbind(expand.grid(head(xs, -1), head(ys, -1)), 
               expand.grid(tail(xs, -1), tail(ys, -1)))[,c(1, 3, 2, 4)]
  colnames(grd) <- c("xmin", "xmax", "ymin", "ymax")
 tibble::as_tibble(grd)
}