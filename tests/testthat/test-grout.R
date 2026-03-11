test_that("grout() tile counts and dangles are correct", {
  .rsch <- function(x) {
    nms <- c("ntilesX", "ntilesY", "dangleX", "dangleY", "blockX", "blockY")
    unname(unlist(x$scheme[nms]))
  }

  ## ncol=8, nrow=5, block=5x8
  expect_equal(.rsch(grout(c(8L, 5L), blocksize = c(5L, 8L))),
               c(2L, 1L, 2L, 3L, 5L, 8L))

  ## ncol=3, nrow=2, block=5x24
  expect_equal(.rsch(grout(c(3L, 2L), blocksize = c(5L, 24L))),
               c(1L, 1L, 2L, 22L, 5L, 24L))

  ## ncol=2, nrow=5, block=5x24
  expect_equal(.rsch(grout(c(2L, 5L), blocksize = c(5L, 24L))),
               c(1L, 1L, 3L, 19L, 5L, 24L))

  ## ncol=20, nrow=50, block=5x24
  expect_equal(.rsch(grout(c(20L, 50L), blocksize = c(5L, 24L))),
               c(4L, 3L, 0L, 22L, 5L, 24L))

  ## large raster
  expect_equal(.rsch(grout(c(6002L, 5001L), blocksize = c(5L, 24L))),
               c(1201L, 209L, 3L, 15L, 5L, 24L))

  expect_equal(.rsch(grout(c(6002L, 5001L), blocksize = c(501L, 23L))),
               c(12L, 218L, 10L, 13L, 501L, 23L))

  ## 1x1 block - one tile per pixel
  expect_equal(.rsch(grout(c(6002L, 5001L), blocksize = c(1L, 1L))),
               c(6002L, 5001L, 0L, 0L, 1L, 1L))

  ## single tile
  expect_equal(.rsch(grout(c(6002L, 5001L), blocksize = c(6002L, 5001L))),
               c(1L, 1L, 0L, 0L, 6002L, 5001L))
})

test_that("grout() validates inputs", {
  expect_error(grout(c(-1L, 10L)))
  expect_error(grout(c(10L, 10L), blocksize = c(0L, 8L)))
  expect_error(grout(c(10L, 10L, 5L)))  ## wrong length dimension
})

test_that("grout() default extent is pixel space", {
  g <- grout(c(10L, 20L))
  ex <- g$scheme$inputraster$extent
  expect_equal(ex, c(0, 10, 0, 20))
})

test_that("tile_index() returns correct dimensions", {
  g <- grout(c(87L, 61L), extent = c(0, 1, 0, 1), blocksize = c(32L, 16L))
  ti <- tile_index(g)
  expect_s3_class(ti, "tbl_df")
  expect_equal(nrow(ti), prod(g$tileraster$dimension))
  expect_named(ti, c("tile","offset_x","offset_y","tile_col","tile_row",
                     "ncol","nrow","xmin","xmax","ymin","ymax"))
})

test_that("tile_index() trims dangle correctly", {
  g  <- grout(c(10L, 10L), extent = c(0, 10, 0, 10), blocksize = c(3L, 4L))
  ti <- tile_index(g)
  ## right column ncol should be 10 mod 3 = 1
  right <- ti[ti$tile_col == max(ti$tile_col), ]
  expect_equal(unique(right$ncol), 10L - 3L * (max(ti$tile_col) - 1L))
  ## bottom row nrow should be 10 mod 4 = 2
  bottom <- ti[ti$tile_row == max(ti$tile_row), ]
  expect_equal(unique(bottom$nrow), 10L - 4L * (max(ti$tile_row) - 1L))
})

test_that("tile_index() offsets are non-overlapping and contiguous in x", {
  g  <- grout(c(20L, 20L), extent = c(0, 20, 0, 20), blocksize = c(6L, 6L))
  ti <- tile_index(g)
  row1 <- ti[ti$tile_row == 1L, ]
  expect_equal(row1$offset_x, c(0L, 6L, 12L, 18L))
})

test_that("print and plot work without error", {
  g <- grout(c(3L, 2L))
  expect_output(print(g))
  expect_silent(plot(g))
})

test_that("tile_spec() returns a data frame with expected columns", {
  ts <- tile_spec(c(8194L, 8194L), c(140, 155, -45, -30), profile = "geodetic")
  expect_s3_class(ts, "data.frame")
  expect_true(all(c("tile","tile_col","tile_row","zoom",
                    "xmin","xmax","ymin","ymax","ncol","nrow","crs") %in% names(ts)))
  expect_equal(unique(ts$crs), "EPSG:4326")
})

test_that("tile_zoom() returns a non-negative integer", {
  z <- tile_zoom(c(8194L, 8194L), c(140, 155, -45, -30), profile = "geodetic")
  expect_true(is.numeric(z))
  expect_true(z >= 0)
})
