% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tiles.R
\name{grout}
\alias{grout}
\title{Create a tiling scheme from a raster}
\usage{
grout(dimension, extent = NULL, blocksize = NULL, projection = NA_character_)
}
\arguments{
\item{dimension}{number of columns and rows of the raster grid}

\item{extent}{extent of the raster grid xmin,xmax,ymin,ymax}

\item{blocksize}{tile dimensions in columns (X) and rows (Y)}

\item{projection}{the projection (crs) of the grid}
}
\value{
A "tile scheme" object with information about the tile spacing and extent.
}
\description{
The input may be an actual raster or a matrix. There is an assumed
block size of 256x256, and the scheme will record the number of tiles
in each dimension and the amount of "overlapping dangle" when the dimensions
of the data don't fit neatly within the tiles.
}
\details{
If extent is not provided the default 'xmin=0,xmax=ncol,ymin=0,ymax=nrow' is used. 

The tile scheme object has print and plot methods for basic information. 

See example in the README with 'wk::rct' to generate plot-able and efficient 
spatial objects from the scheme.
}
\examples{
## one block tile (too big)
grout(c(87, 61), blocksize = c(256L, 256L))
## more size appropriate
grout(c(87, 61), blocksize = c(8, 8))
grout(c(10, 20), c(0, 1, 0, 2), blocksize = c(256, 256))
}
