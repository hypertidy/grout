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

test_that("tile_index(tile =) matches subsetting the full index", {
  schemes <- list(
    grout(c(87L, 61L), extent = c(0, 1, 0, 1), blocksize = c(32L, 16L)),
    grout(c(10L, 10L), blocksize = c(3L, 4L)),
    grout(c(15L, 13L), extent = c(0, 15, 0, 13), blocksize = c(4L, 4L)),
    grout(c(61L, 87L), blocksize = c(61L, 87L)),   ## single tile
    grout(c(256L, 1L), blocksize = c(16L, 16L))    ## single tile row
  )
  for (g in schemes) {
    full <- tile_index(g)
    n    <- nrow(full)
    ## every single tile, one at a time
    for (i in seq_len(min(n, 40L))) {
      expect_equal(tile_index(g, tile = i), full[i, ])
    }
    ## unordered, repeated, and reversed selections
    ii <- unique(c(n, 1L, max(1L, n %/% 2L)))
    expect_equal(tile_index(g, tile = ii), full[ii, ])
    expect_equal(tile_index(g, tile = c(1L, 1L, n)), full[c(1L, 1L, n), ])
    expect_equal(tile_index(g, tile = rev(seq_len(n))), full[rev(seq_len(n)), ])
  }
})

test_that("tile_index(tile =) validates and handles the empty selection", {
  g <- grout(c(10L, 10L), blocksize = c(3L, 4L))
  n <- prod(g$tileraster$dimension)
  expect_error(tile_index(g, tile = 0L))
  expect_error(tile_index(g, tile = n + 1L))
  expect_error(tile_index(g, tile = NA_integer_))
  ti <- tile_index(g, tile = integer(0))
  expect_equal(nrow(ti), 0L)
  expect_named(ti, names(tile_index(g)))
})

test_that("tile_index() covers the source grid exactly", {
  for (bs in list(c(3L, 4L), c(12L, 16L), c(256L, 256L))) {
    g  <- grout(c(87L, 61L), blocksize = bs)
    ti <- tile_index(g)
    expect_equal(sum(ti$ncol * ti$nrow), 87L * 61L)
  }
})

test_that("tile_index(clip = FALSE) reports whole blocks", {
  for (bs in list(c(3L, 4L), c(12L, 16L), c(32L, 16L), c(256L, 256L))) {
    g  <- grout(c(87L, 61L), blocksize = bs)
    ti <- tile_index(g, clip = FALSE)
    expect_true(all(ti$ncol == bs[1L]))
    expect_true(all(ti$nrow == bs[2L]))
    ## clipped and unclipped differ only at the margin, and only by the dangle
    tc <- tile_index(g)
    expect_equal(unique((ti$ncol - tc$ncol)[tc$tile_col == max(tc$tile_col)]),
                 g$scheme$dangleX)
    expect_equal(unique((ti$nrow - tc$nrow)[tc$tile_row == max(tc$tile_row)]),
                 g$scheme$dangleY)
    ## interior tiles are untouched
    interior <- tc$tile_col < max(tc$tile_col) & tc$tile_row < max(tc$tile_row)
    expect_equal(ti[interior, ], tc[interior, ])
  }
})

test_that("tile_index(clip = FALSE) tiles the tile extent exactly", {
  g  <- grout(c(87L, 61L), extent = c(0, 1, 0, 1), blocksize = c(12L, 16L))
  ti <- tile_index(g, clip = FALSE)
  tex <- g$tileraster$extent
  expect_equal(min(ti$xmin), tex[1L])
  expect_equal(max(ti$xmax), tex[2L])
  expect_equal(min(ti$ymin), tex[3L])
  expect_equal(max(ti$ymax), tex[4L])
  ## and the areas sum to the tiled area (no gaps, no overlaps)
  expect_equal(sum((ti$xmax - ti$xmin) * (ti$ymax - ti$ymin)),
               diff(tex[1:2]) * diff(tex[3:4]))
})

test_that("clip interacts correctly with tile =", {
  g <- grout(c(10L, 10L), blocksize = c(3L, 4L))
  n <- prod(g$tileraster$dimension)
  full <- tile_index(g, clip = FALSE)
  for (i in seq_len(n)) {
    expect_equal(tile_index(g, tile = i, clip = FALSE), full[i, ])
  }
})

test_that("tiles_from_extent() round-trips every tile", {
  schemes <- list(
    grout(c(87L, 61L), blocksize = c(12L, 16L)),
    grout(c(87L, 61L), extent = c(0, 1, 0, 1), blocksize = c(32L, 16L)),
    grout(c(10L, 10L), blocksize = c(3L, 4L)),
    grout(c(15L, 13L), extent = c(0, 15, 0, 13), blocksize = c(4L, 4L)),
    grout(c(61L, 87L), blocksize = c(61L, 87L)),
    grout(c(3600L, 1800L), extent = c(-180, 180, -90, 90), blocksize = c(256L, 256L))
  )
  for (g in schemes) {
    for (clip in c(TRUE, FALSE)) {
      ti <- tile_index(g, clip = clip)
      for (i in seq_len(nrow(ti))) {
        ex <- c(ti$xmin[i], ti$xmax[i], ti$ymin[i], ti$ymax[i])
        expect_equal(tiles_from_extent(g, ex), ti$tile[i])
      }
    }
  }
})

test_that("tiles_from_extent() handles whole, outside, oversized and point regions", {
  g   <- grout(c(87L, 61L), blocksize = c(12L, 16L))
  n   <- prod(g$tileraster$dimension)
  tex <- g$tileraster$extent
  w   <- diff(tex[1:2])

  ## the tiled extent, and the source extent, are both every tile
  expect_equal(tiles_from_extent(g, tex), seq_len(n))
  expect_equal(tiles_from_extent(g, g$scheme$inputraster$extent), seq_len(n))

  ## oversized is clamped, not an error
  expect_equal(tiles_from_extent(g, tex + c(-w, w, -w, w)), seq_len(n))

  ## entirely outside
  expect_equal(tiles_from_extent(g, c(tex[2] + w, tex[2] + 2 * w, tex[3], tex[4])),
               integer(0))

  ## half open: a point on an interior tile boundary belongs to the right/lower tile
  ti <- tile_index(g, clip = FALSE)
  b  <- ti$xmax[1L]
  expect_equal(tiles_from_extent(g, c(b, b, ti$ymin[1L], ti$ymax[1L])), 2L)

  ## a region spanning two tiles in each direction gives four tiles
  ntx <- g$tileraster$dimension[1L]
  ex2 <- c(ti$xmin[1L], ti$xmax[2L], ti$ymin[ntx + 2L], ti$ymax[1L])
  expect_equal(tiles_from_extent(g, ex2), sort(c(1L, 2L, ntx + 1L, ntx + 2L)))
})

test_that("tiles_from_extent() validates its extent", {
  g <- grout(c(10L, 10L), blocksize = c(3L, 4L))
  expect_error(tiles_from_extent(g, c(0, 1, 0)))
  expect_error(tiles_from_extent(g, c(0, 1, 0, NA)))
  expect_error(tiles_from_extent(g, c(1, 0, 0, 1)))   ## xmin > xmax
  expect_error(tiles_from_extent(g, c(0, 1, 1, 0)))   ## ymin > ymax
})
