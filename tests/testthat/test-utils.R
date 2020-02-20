test_that("utils works", {
  expect_equal(dim(raster0(matrix(1:12, 3), transpose = TRUE)), c(3L, 4L, 1L))
  
  expect_equal(dim(raster0(matrix(1:12, 3), transpose = FALSE)), c(4L, 3L, 1L))
  
  expect_equal(raster0(matrix(1:12, 3), transpose = TRUE)@extent@xmin, 0L)
  expect_equal(raster0(matrix(1:12, 3), transpose = TRUE)@extent@xmax, 4L)
  expect_equal(raster0(matrix(1:12, 3), transpose = FALSE)@extent@xmax, 3L)
  
  expect_equal(raster0(matrix(1:12, 3), transpose = FALSE)@extent@ymin, 0L)
  
  expect_equal(raster0(matrix(1:12, 3), transpose = TRUE)@extent@ymax, 3L)
  
  expect_equal(raster0(matrix(1:12, 3), transpose = FALSE)@extent@ymax, 4L)
  
})
