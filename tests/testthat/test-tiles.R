library(raster)
r0 <- raster(matrix(0, 5, 8))
r1 <- raster(matrix(0, 2, 3))
r2 <- raster(matrix(0, 5, 2))
r3 <- raster(matrix(0, 50, 20))

r4 <- raster(matrix(0, 5001, 6002))
nms <- c("ntilesX", "ntilesY", 
         "dangleX", 
         "dangleY", 
         "blockX", 
         "blockY")
## raw scheme of the tiling
.rsch <- function(x) unname(unlist(x$scheme[nms]))
test_that("tiles works", {
  ## also integers
 expect_equal(.rsch(tiles(r0, 5, 8)), 
              c(1L, 1L, 3L, 3L, 5L, 8L))
  
  expect_equal(.rsch(tiles(r1, 5, 24)), 
               c(1L, 1L, 2L, 22L, 5L, 24L))
  
  expect_equal(.rsch(tiles(r2, 5, 24)), 
               c(1L, 1L, 3L, 19L, 5L, 24L))
  
  
  expect_equal(.rsch(tiles(r3, 5, 24)), 
               c(4L, 2L, 0L, 2L, 5L, 24L))
  
  expect_equal(.rsch(tiles(r4, 5, 24)), 
               c(1200L, 208L, 2L, 9L, 5L, 24L))
  
  
  expect_equal(.rsch(tiles(r4, 501, 23)), 
               c(11L, 217L, 491L, 10L, 501L, 23L))
  
  expect_equal(.rsch(tiles(r4, 1, 1)), 
               c(6002L, 5001L, 0L, 0L, 1L, 1L))
  
  expect_equal(.rsch(tiles(r4, 6002, 5001)), 
               c(1L, 1L, 0L, 0L, 6002L, 5001L))
  
  
  
})

test_that("coercion works", {
  expect_s4_class(as_polys(tiles(r2)), "SpatialPolygonsDataFrame")
})

test_that("print and plot works", {
  expect_output(print(tiles(r1)))
  expect_silent(plot(tiles(r1)))
})