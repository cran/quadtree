% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extent.R
\name{extent}
\alias{extent}
\alias{extent,Quadtree-method}
\alias{extent.Quadtree}
\title{Get the extent of a \code{Quadtree}}
\usage{
\S4method{extent}{Quadtree}(x, original = FALSE)
}
\arguments{
\item{x}{a \code{\link{Quadtree}}}

\item{original}{boolean; if \code{FALSE} (the default), it returns the total
extent covered by the quadtree. If \code{TRUE}, the function returns the
extent of the original raster used to create the quadtree, before the
dimensions were adjusted by padding with \code{NA}s and/or the raster was
resampled.}
}
\value{
an \code{\link[raster:Extent-class]{Extent}} object
}
\description{
Gets the extent of the \code{\link{Quadtree}} as an
  \code{\link[raster:Extent-class]{Extent}} object (from the raster package).
}
\examples{
library(quadtree)
habitat <- terra::rast(system.file("extdata", "habitat.tif", package="quadtree"))

# create a quadtree
qt <- quadtree(habitat, split_threshold = .1, adj_type = "expand")

# retrieve the extent and the original extent
ext <- extent(qt)
ext_orig <- extent(qt, original = TRUE)

ext
ext_orig

# plot them
plot(qt)
rect(ext[1], ext[3], ext[2], ext[4], border = "blue", lwd = 4)
rect(ext_orig[1], ext_orig[3], ext_orig[2], ext_orig[4],
     border = "red", lwd = 4)
}
