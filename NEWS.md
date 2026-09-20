# grout 0.1.0.9001

* `tile_index()` gains a `tile` argument, to evaluate the index at chosen
  tile indices rather than materializing every row.  `tile_index(x, tile = i)`
  is identical to `tile_index(x)[i, ]`, and `i` may be unordered or repeated.
  The default `tile = NULL` is unchanged.

* `tile_index()` now marks dangle tiles by their tile column and row rather
  than by looking up global tile cell numbers, which is what allows an
  arbitrary `tile` selection.  Output for the full index is unchanged.
  `vaster::cell_from_col()` and `vaster::cell_from_row()` are no longer used.

# grout 0.1.0

* `grout()` is now the definitive user constructor; removed internal
  `.groutfrom()` wrapper and dead `dimXY` compatibility branch.

* `.tilescheme()` simplified: tile counts now computed with `ceiling()`
  for clarity; removed duplicate "hack" comment logic.

* `print.grout_tiles()` output uses `x` format for dimensions and now
  returns `x` invisibly (not `NULL`).

* `plot.grout_tiles()` rewritten to iterate individual tile rectangles
  rather than relying on an unexported `as_rect` S3 method.

* `as_rect_grout()` is now an internal helper (not an S3 generic);
  `.p2s()` (unused) removed.

* `extent.grout_tilescheme()` replaced by internal `.tilescheme_extent()`.

* `tile_spec()` and `tile_zoom()` cleaned up: explicit `as.integer()`
  coercions, removed `Remotes:` dependency on dev vaster, informational
  messages improved.

* Comprehensive test suite added covering dangle trimming, contiguous
  offsets, input validation, and `tile_spec()`/`tile_zoom()` output.

* DESCRIPTION bumped to `R (>= 3.5.0)`, added `Config/testthat/edition: 3`,
  removed `Remotes:` field, added `wk` to `Suggests`.

# grout 0.0.2.9010 (dev)

* New function `tile_spec()` to generate tile specification from input
  extent and dimension at a given zoom. New function `tile_zoom()` returns
  the natural maximum zoom value.

* Removed all spatial input coercions; just use dimension + extent.

* Added extent columns to `tile_index()`.

* Removed raster dependency; uses vaster for basic grid/cell logic.
