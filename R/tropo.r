#' Prepare topo source
#'
#' 
#' @export
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

.tilescheme <- function(x, blockX = 256, blockY = 256) {
  dm <- dim(x)
  ntilesX <- dm[2L] %/% blockX
  dangleX <- dm[2L] %% blockX
  ntilesY <- dm[1L] %/% blockY
  dangleY <- dm[1L] %% blockY
  if (dangleX > 0) ntilesX <- ntilesX + 1
  if (dangleY > 0) ntilesY <- ntilesY + 1
  
  list(inputraster = raster(x), 
       ntilesX = ntilesX, ntilesY = ntilesY, 
       dangleX = dangleX, dangleY = dangleY, 
       blockX = blockX, blockY = blockY)

}

extent.tilescheme <- function(x) {
  extent(xmin(x$inputraster), 
         xmax(x$inputraster) +  (x$blockX - x$dangleX) * res(x$inputraster)[1], 
         ymin(x$inputraster) -  (x$blockY - x$dangleY) * res(x$inputraster)[2], 
         ymax(x$inputraster) )
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
tiles <- function(x, blockX = 256, blockY = 256) {
  ts <- .tilescheme(x, blockX = blockX, blockY = blockY)
  rt <- raster(ncol = ts$ntilesX, nrow = ts$ntilesY)
  extent(rt) <- extent.tilescheme(ts)
  pp <- as(rt, "SpatialPolygonsDataFrame")
  pp$tile <- seq(ncell(rt))
  pp$layer <- NULL
  list(tileraster = setValues(rt, seq(ncell(rt))), poly = pp)
}
