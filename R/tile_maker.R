
## level 0 we are in the one tile
## base tile is dim = 256x256 global, so consider this a raster with dim * 2^zoom
tile_maker <- function(dimension, extent, zoom = NULL, blocksize = c(256L, 256L), 
                       profile = c("mercator", "geodetic", "raster"), max_zoom = 24) {
  profile <- match.arg(profile)
  autozoom <- FALSE
  if (is.null(zoom)) {
    autozoom <- TRUE
    resolution0 <- diff(extent)[c(1, 3)]/dimension
   
    ## we need to check if we can go further, we aren't using the zoom values
    zoom <- sample(letters, max_zoom, replace = TRUE) 
  }
  global <- switch(profile, 
                   mercator = c(-1, 1, -1, 1) * 20037508.342789244, 
                   geodetic = c(-180, 180, -90, 90), 
                   raster = extent)
  transform <- c(global[1], diff(global[1:2])/blocksize[1], 0, 
                 global[4], 0, -diff(global[3:4])/blocksize[2])
 
  if (autozoom)  zvalue <- -1
  l <- list()
  ## we have a snapped grid and the right dimensions for this zoom
  for (i in seq_along(zoom)) {
   if (autozoom) zvalue <- zvalue + 1 else zvalue <- zoom[i]

   idimension <- blocksize * 2^zvalue / blocksize
    v <- vcrop(extent, idimension, global, snap = "out")
    col <- col_from_x(idimension, global, v$extent[1:2])
    row <- row_from_y(idimension, global, v$extent[3:4])
  
    ## the tiles we need at this zoom are the expanded grid of col and row sequence
    xs <- seq(col[1], col[2])
    ys <-  rep(seq(row[1], row[2]), each = length(xs))  
    index <- cell_from_row_col(idimension, ys, xs)
    ## switch to 0-based
    tiles <- cbind(tile = index, tile_col = xs, tile_row = ys) - 1
    tilesize <- abs(transform[c(2, 6)]) * blocksize 
    tt <- tibble::as_tibble(tiles) |> dplyr::mutate(zoom = zvalue,
                                              xmin =  transform[1] + tile_col * tilesize[1], 
                                              xmax =  transform[1] + (tile_col +1) * tilesize[1], 
                                              ymin = transform[4] -  (tile_row + 1)* tilesize[2]  , 
                                              ymax = transform[4] -  (tile_row) * tilesize[2])
                                           
  l <-   c(l, list(tt))    
  if (autozoom && max(transform[c(2, 6)]) < min(resolution0)) break; 
  transform[c(2, 6)] <- transform[c(2, 6)] / 2

  }  
  do.call(rbind, l)
}



