#' Prepare topo source
#'
#' @noRd
#' @importFrom raster raster extent xmin xmax ymin ymax
tropo <- function() {
  file <- .etopo2()
  ## check if exists
  if (!file.exists(file)) {
    stop("no raadsync yet")
    #do_raadsync()
  } 
  r <- raster(file)
  
  tiles(r)
}

# from a raster or matrix, tile out
# the dimensions based on blockX/blockY
.tilescheme <- function(x, blockX = 256, blockY = 256) {
  dm <- dim(x)
  if (dm[2L] <= blockX) {
    ntilesX <- 1L
    dangleX <- blockX - dm[2L] 
  } else {
    ntilesX <- dm[2L] %/% blockX
    dangleX <- dm[2L] %% blockX
  }

  if (dm[1L] <= blockY) {
    ntilesY <- 1L
    dangleY <- blockY - dm[1L] 
  } else {
    ntilesY <- dm[1L] %/% blockY
    dangleY <- dm[1L] %%  blockY
  }
 
  structure(list(inputraster = raster(x), 
       ntilesX = ntilesX, ntilesY = ntilesY, 
       dangleX = dangleX, dangleY = dangleY, 
       blockX = blockX, blockY = blockY), 
       class = "tropo_tilescheme")

}

extent.tropo_tilescheme <- function(x) {
  raster::extent(raster::xmin(x$inputraster), 
                 raster::xmax(x$inputraster) +  (x$blockX - x$dangleX) * raster::res(x$inputraster)[1], 
                 raster::ymin(x$inputraster) -  (x$blockY - x$dangleY) * raster::res(x$inputraster)[2], 
                 raster::ymax(x$inputraster) )
}

#' tiles of a raster
#'
#' @param x a raster
#' @param blockX tile dimensions in columns (X)
#' @param blockY tile diemnsions in rows (Y)
#'
#' @return A raster layer with cells having the tile number and extent releant to the input. 
#' @export
#'
#' @examples
#' rv <- tropo:::raster0(volcano)
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
  rt <- raster::raster(extent.tropo_tilescheme(ts), 
                       ncol = ts$ntilesX, nrow = ts$ntilesY)
  structure(list(tileraster = raster::setValues(rt, seq(raster::ncell(rt))),
                 scheme = ts),
            class = "tropo_tiles")
}

#' print tiles
#' @param x a tropo [tiles()] object
#' @param ... ignored
#' @export
print.tropo_tiles <- function(x, ...) { 
  dm <- dim(x$tileraster)
  ex <- raster::extent(x$tileraster)
  cat(sprintf("tropo tiles: %i\n", prod(dm)))
  cat(sprintf("    x tiles: %i\n", dm[2L]) )
  cat(sprintf("    y tiles: %i\n", dm[1L]))
  cat(sprintf("     blockX: %i\n", x$scheme$blockX) )
  cat(sprintf("     blockY: %i\n", x$scheme$blockX))
  
  cat(sprintf("     dangle: %i,%i (x,y)\n", x$scheme$dangleX, x$scheme$dangleY))
  invisible(NULL)
}

#' plot tiles
#' 
#' @param x a tropo [tiles()] object
#' @param ... arguments passed to methods (particularly [sp::plot()])
#'
#' @importFrom graphics plot
#' @export
plot.tropo_tiles <- function(x, ...) {
  plot(as_polys(x), ...)
  raster::plot(raster::extent(x$scheme$inputraster), add = TRUE, col = "firebrick", lty = 2)
  invisible(NULL)
}


#' Tiles as polygons
#' 
#' @param x a tropo [tiles()] object 
#' @param ... ignored
#'
#' @export
as_polys <- function(x, ...) {
  UseMethod("as_polys")
}
#' @name as_polys
#' @importFrom methods as
#' @export
as_polys.tropo_tiles <- function(x, ...) {
  pp <- as(x$tileraster, "SpatialPolygonsDataFrame")
  pp$tile <- seq(raster::ncell(x$tileraster))
  pp$layer <- NULL
  pp
}