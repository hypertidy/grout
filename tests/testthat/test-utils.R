## we are removing raster so no tests will work

# library(raster)
# r0 <- raster(matrix(0, 5, 8))
# r1 <- raster(matrix(0, 2, 3))
# r2 <- raster(matrix(0, 5, 2))
# r3 <- raster(matrix(0, 50, 20))
# 
# r4 <- raster(matrix(0, 5001, 6002))
# nms <- c("ntilesX", "ntilesY", 
#          "dangleX", 
#          "dangleY", 
#          "blockX", 
#          "blockY")
# ## raw scheme of the tiling
# .rsch <- function(x) unname(unlist(x$scheme[nms]))
# test_that("tiles works", {
#   ## also integers
#  expect_equal(.rsch(tiles(r0, 5, 8)), 
#               c(2L, 1L, 2L, 3L, 5L, 8L))
#   
#   expect_equal(.rsch(tiles(r1, 5, 24)), 
#                c(1L, 1L, 2L, 22L, 5L, 24L))
#   
#   expect_equal(.rsch(tiles(r2, 5, 24)), 
#                c(1L, 1L, 3L, 19L, 5L, 24L))
#   
#   
#   expect_equal(.rsch(tiles(r3, 5, 24)), 
#                c(4L, 3L, 0L, 22L, 5L, 24L))
#   
#   expect_equal(.rsch(tiles(r4, 5, 24)), 
#                c(1201L, 209L, 3L, 15L, 5L, 24L))
#   
#   
#   expect_equal(.rsch(tiles(r4, 501, 23)), 
#                c(12L, 218L, 10L, 13L, 501L, 23L))
#   
#   expect_equal(.rsch(tiles(r4, 1, 1)), 
#                c(6002L, 5001L, 0L, 0L, 1L, 1L))
#   
#   expect_equal(.rsch(tiles(r4, 6002, 5001)), 
#                c(1L, 1L, 0L, 0L, 6002L, 5001L))
#   
#   
#   ## internal template should not store values (light raster)
#   expect_true(!raster::hasValues(tiles(r4)$tileraster))
#   expect_true(!raster::hasValues(tiles(r4)$scheme$inputraster))
# })
# 
# test_that("coercion works", {
#   expect_s4_class(as_polys(tiles(r2)), "SpatialPolygonsDataFrame")
# })
# 
# test_that("print and plot works", {
#   expect_output(print(tiles(r1)))
#   expect_silent(plot(tiles(r1)))
# })



# raster0 <- function (x, transpose = TRUE) {
# #  x <- t(x[, raster::ncol(x):1])
#   if (transpose) {
#     e <- raster::extent(0, raster::ncol(x), 0, raster::nrow(x))
#     
#   }
#   else {
#     e <- raster::extent(0, raster::nrow(x), 0, raster::ncol(x))
#     x <- t(x[, raster::ncol(x):1])
#     
#   }
#   raster::raster(raster::setExtent(raster::raster(x), e))
# }
# 
# 
# 
# test_that("utils works", {
#   expect_equal(dim(raster0(matrix(1:12, 3), transpose = TRUE)), c(3L, 4L, 1L))
#   
#   expect_equal(dim(raster0(matrix(1:12, 3), transpose = FALSE)), c(4L, 3L, 1L))
#   
#   expect_equal(raster0(matrix(1:12, 3), transpose = TRUE)@extent@xmin, 0L)
#   expect_equal(raster0(matrix(1:12, 3), transpose = TRUE)@extent@xmax, 4L)
#   expect_equal(raster0(matrix(1:12, 3), transpose = FALSE)@extent@xmax, 3L)
#   
#   expect_equal(raster0(matrix(1:12, 3), transpose = FALSE)@extent@ymin, 0L)
#   
#   expect_equal(raster0(matrix(1:12, 3), transpose = TRUE)@extent@ymax, 3L)
#   
#   expect_equal(raster0(matrix(1:12, 3), transpose = FALSE)@extent@ymax, 4L)
#   
# })
