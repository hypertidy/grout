## separate the work for one zoom from this function
## generalize mercator() and geodetic/longlat() profile as a global spec, the input
## raster can be relative to a different grid as well
## make sure out of bounds extents don't break the profile logic


## level 0 we are in the one tile
## base tile is dim = 256x256 global, so consider this a raster with dim * 2^zoom
#zoom stuff  
#resolution0 <- diff(extent)[c(1, 3)]/dimension
   
#    transform[c(2, 6)] <- transform[c(2, 6)] / 2^zoom
#  if (autozoom && max(transform[c(2, 6)]) < min(resolution0)) break; 
#  if (autozoom) transform[c(2, 6)] <- transform[c(2, 6)] / 2

 
#' Create tile specification
#'
#' Take an input grid (dimension and extent) and create a specification of
#' the tiling for that grid within a profile. 
#' 
#' Profiles are 'mercator' or "geodetic" for global systems, or can 
#' use "raster" which will use the input grid specification as the entire domain. 
#' 
#' 'tile_row' in output is in TMS orientation (zero is at the bottom), use 
#' 'xyz' arg to switch. 
#'
#' Function `tile_zoom()` will return the "natural" maximum zoom level, i.e. 
#' the zoom at which the tile resolution is just below the input resolution. 
#' Note that no reprojection is done, the input extent must match the profile chosen (use 'raster' for native profile). 
#' @param dimension size in pixels ncol,nrow
#' @param extent xmin,xmax,ymin,ymax
#' @param zoom the zoom level, starts at 0 and can be up to 24
#' @param blocksize size of each tile, defaults to 256x256
#' @param profile profile domain to use, see Details
#' @param xyz default is `FALSE`, if `TRUE` use xyz-mode (zero is at the top)
#'
#' @return data frame with tile specification, tile index, tile_col, tile_row, 
#'  ncol, nrow, xmin, xmax, ymin, ymax, crs
#' @export
#' @importFrom vaster col_from_x row_from_y cell_from_row_col vcrop
#' @examples
#' tile_spec(c(8194, 8194), c(140, 155, -45, -30), profile = "geodetic")
#' 
#' tile_spec(c(2048, 248), c(140, 155, -45, -30), zoom = 5, profile = "geodetic", 
#'    blocksize = c(512, 512))
tile_spec <- function(dimension, extent, zoom = 0, blocksize = c(256L, 256L), 
                       profile = c("mercator", "geodetic", "raster"), 
                      xyz = FALSE) {
  profile <- match.arg(profile)
  
  if (any(diff(extent)[c(1, 3)] < 360) && profile == "mercator") {
    message("very small region for Mercator, is this geodetic (longlat) extent? \n(use 'profile = \"geodetic\"'")
  } 
  global <- switch(profile, 
                   mercator = c(-1, 1, -1, 1) * 20037508.342789244, 
                   geodetic = c(-180, 180, -90, 90), 
                   raster = extent)
  crs <- switch(profile, 
                mercator = "EPSG:3857", 
                geodetic = "EPSG:4326", 
                raster = NA_character_)
  transform <- c(global[1], diff(global[1:2])/blocksize[1], 0, 
                 global[4], 0, -diff(global[3:4])/blocksize[2])
  
  transform[c(2, 6)] <- transform[c(2, 6)] / 2^zoom
  idimension <- blocksize * 2 ^ zoom
  
  v <- vcrop(extent, idimension, global, snap = "out")
  
  col <- col_from_x(idimension, global, v$extent[1:2]) %/% blocksize[1]
  row <- row_from_y(idimension, global, v$extent[3:4]) %/% blocksize[2]
  maxcolrow <- idimension / blocksize
  
  
  xs <- seq(col[1], col[2])
  ys <-  seq(row[1], row[2]) 
  
  if (maxcolrow[1] > 0) {
   xs <- setdiff(xs, maxcolrow[1])
  }
  if (maxcolrow[2] > 0) {
    
  ys <- setdiff(ys, maxcolrow[2])
  }
  ys <- rep(ys,  each = length(xs))
  ## switch to 0-based
  index <- cell_from_row_col(idimension %/% blocksize, ys + 1, xs + 1) - 1
  
  
  tiles <- cbind(tile = index, tile_col = xs, tile_row = ys) 
  tilesize <- abs(transform[c(2, 6)]) * blocksize 
  
  tt <- as.data.frame(tiles) 
  tt$zoom <- zoom
  tt$xmin <-   transform[1] + tt$tile_col * tilesize[1] 
  tt$xmax <- transform[1] + (tt$tile_col +1) * tilesize[1]
  tt$ymin <- transform[4] -  (tt$tile_row + 1)* tilesize[2]
  tt$ymax <- transform[4] -  (tt$tile_row) * tilesize[2]
  ## now that we have calculated each tile extent, switch to TMS mode
  if (!xyz) {
    tt$tile_row <- idimension[2] %/% blocksize[2] - tt$tile_row
  }
  tt$ncol <- blocksize[1]
  tt$nrow <- blocksize[2]
  tt$crs <- crs
  tt
}

#' @export
#' @name tile_spec
tile_zoom <- function(dimension, extent, blocksize = c(256L, 256L), 
                      profile = c("mercator", "geodetic", "raster")) {
  
  
  
  profile <- match.arg(profile)
  
  if (any(diff(extent)[c(1, 3)] < 360) && profile == "mercator") {
    message("very small region for Mercator, is this geodetic (longlat) extent? \n(use 'profile = \"geodetic\"'")
  } 
  global <- switch(profile, 
                   mercator = c(-1, 1, -1, 1) * 20037508.342789244, 
                   geodetic = c(-180, 180, -90, 90), 
                   raster = extent)
  crs <- switch(profile, 
                mercator = "EPSG:3857", 
                geodetic = "EPSG:4326", 
                raster = NA_character_)
  transform <- c(global[1], diff(global[1:2])/blocksize[1], 0, 
                 global[4], 0, -diff(global[3:4])/blocksize[2])
  idimension <- blocksize 
  
  v <- vcrop(extent, idimension, global, snap = "out")
  
  nativeresolution <- diff(extent)[c(1, 3)]/dimension
  for (zoom in 0:23) {

    if (all(abs(transform[c(2, 6)]) < nativeresolution)) return(zoom)
    transform[c(2, 6)] <- transform[c(2, 6)] / 2
   idimension <- blocksize * 2 ^ zoom
  
    v <- vcrop(extent, idimension, global, snap = "out")
  }
  zoom
} 

