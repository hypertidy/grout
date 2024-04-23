## we are removing raster so no tests will work

# library(raster)
# r0 <- raster(matrix(0, 5, 8))
# r1 <- raster(matrix(0, 2, 3))
# r2 <- raster(matrix(0, 5, 2))
# r3 <- raster(matrix(0, 50, 20))
# 
r0_dm <- c(8, 5)
r1_dm <- c(3, 2)
r2_dm <- c(2, 5)
r3_dm <- c(20, 50)
r4_dm <- c(6002, 5001)
# r4 <- raster(matrix(0, 5001, 6002))
 nms <- c("ntilesX", "ntilesY", 
          "dangleX", 
          "dangleY", 
          "blockX", 
          "blockY")
# ## raw scheme of the tiling
.rsch <- function(x) unname(unlist(x$scheme[nms]))
test_that("tiles works", {
#   ## also integers
 expect_equal(.rsch(grout(r0_dm, blocksize = r0_dm[2:1])), 
               c(2L, 1L, 2L, 3L, 5L, 8L))
#   
   expect_equal(.rsch(grout(r1_dm, blocksize = c(5, 24))), 
                c(1L, 1L, 2L, 22L, 5L, 24L))

  expect_equal(.rsch(grout(r2_dm, blocksize = c(5, 24))),
               c(1L, 1L, 3L, 19L, 5L, 24L))
  


  expect_equal(.rsch(grout(r3_dm, blocksize = c(5, 24))),
               c(4L, 3L, 0L, 22L, 5L, 24L))

  expect_equal(.rsch(grout(r4_dm, blocksize = c(5, 24))),
               c(1201L, 209L, 3L, 15L, 5L, 24L))


  expect_equal(.rsch(grout(r4_dm, blocksize = c(501, 23))),
               c(12L, 218L, 10L, 13L, 501L, 23L))

  expect_equal(.rsch(grout(r4_dm, blocksize = c(1, 1))),
               c(6002L, 5001L, 0L, 0L, 1L, 1L))

  expect_equal(.rsch(grout(r4_dm, blocksize = c(6002, 5001))),
               c(1L, 1L, 0L, 0L, 6002L, 5001L))

})
# 
# test_that("coercion works", {
#   expect_s4_class(as_polys(grout(r2_dm)), "SpatialPolygonsDataFrame")
# })
# 
test_that("print and plot works", {
  expect_output(print(grout(r1_dm)))
  expect_silent(plot(grout(r1_dm)))
})



