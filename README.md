
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/hypertidy/grout.svg?branch=master)](https://travis-ci.org/hypertidy/grout)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/hypertidy/grout?branch=master&svg=true)](https://ci.appveyor.com/project/hypertidy/grout)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/grout)](https://CRAN.R-project.org/package=grout)
<!-- badges: end -->

# grout

Abstract tiling schemes.

Given a grid, impose a tiling scheme. The dangle is overlap the tiles
impose (in pixels).

We use the term “block” to refer to the size of each tile in pixels.

Consider a raster 16 x 12, with 4 x 4 tiling there is no overlap.

``` r
library(grout)
r0 <- raster::raster(matrix(0, 12, 16), xmn = 0, xmx = 16, ymn = 0, ymx = 12)
tiles(r0, blockX = 4, blockY = 4)
#>           tiles: 4, 3 (x * y = 12)
#>           block: 4, 4 
#>          dangle: 0, 0 
#> tile resolution: 4, 4 
#>     tile extent: 0, 16, 0, 12 (xmin,xmax,ymin,ymax)
#>           grain: 1, 1 (4 : x, 4 : y)
```

But, if our raster has an dimension that doesn’t divide neatly into the
block size, then there is some tile overlap. This should work for any
raster dimension and any arbitrary tile size.

``` r
r1 <- raster::raster(matrix(0, 13, 15), xmn = 0, xmx = 15, ymn = 0, ymx = 13)
tiles(r1, 4, 4)
#>           tiles: 4, 4 (x * y = 16)
#>           block: 4, 4 
#>          dangle: 1, 3 
#> tile resolution: 4, 4 
#>     tile extent: 0, 16, -3, 13 (xmin,xmax,ymin,ymax)
#>           grain: 1, 1 (4 : x, 4 : y)
```

``` r

r <- raster::raster(matrix(1:12, 30, 44), xmn= 0,xmx = 4, ymn = 0, ymx = 3)


(t1 <- tiles(r, blockX = 12, blockY = 12))
#>           tiles: 4, 3 (x * y = 12)
#>           block: 12, 12 
#>          dangle: 4, 6 
#> tile resolution: 1.090909, 1.2 
#>     tile extent: 0, 4.363636, -0.6, 3 (xmin,xmax,ymin,ymax)
#>           grain: 0.09090909, 0.1 (12 : x, 12 : y)
plot(t1)
```

![](man/figures/README-grid-1.png)<!-- -->

``` r

(t2 <- tiles(volcano, 12, 16))
#>           tiles: 6, 6 (x * y = 36)
#>           block: 12, 16 
#>          dangle: 11, 9 
#> tile resolution: 0.1967213, 0.183908 
#>     tile extent: 0, 1.180328, -0.1034483, 1 (xmin,xmax,ymin,ymax)
#>           grain: 0.01639344, 0.01149425 (12 : x, 16 : y)

plot(t2)
```

![](man/figures/README-grid-2.png)<!-- -->

We can generate polygons from these.

``` r
p <- as_polys(t1)

sp::plot(p)
```

![](man/figures/README-poly-1.png)<!-- -->

Or just plot the scheme.

``` r
plot(t1)
```

![](man/figures/README-plot-1.png)<!-- -->

-----

Please note that the ‘grout’ project is released with a [Contributor
Code of
Conduct](https://github.com/hypertidy/grout/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
