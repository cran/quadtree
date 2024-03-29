#' @include generics.R

#' @name extract
#' @aliases extract,Quadtree,ANY-method extract.Quadtree
#' @title Extract \code{Quadtree} values
#' @description Extracts the cell values and optionally the cell extents at the
#'   given points.
#' @param x a \code{\link{Quadtree}}
#' @param y a two-column matrix representing point coordinates. First column
#'   contains the x-coordinates, second column contains the y-coordinates
#' @param extents boolean; if \code{FALSE} (the default), a vector containing
#'   cell values is returned. If \code{TRUE}, a matrix is returned providing
#'   each cell's extent in addition to its value
#' @return
#' If \code{extents = FALSE}, returns a numeric vector corresponding to the
#' values at the points represented by \code{pts}.
#'
#' If \code{extents = TRUE}, returns a six-column numeric matrix providing the
#' extent of each cell along with the cell's value and ID. The six columns are,
#' in this order: \code{id}, \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax},
#' \code{value}.
#' @examples
#' library(quadtree)
#' habitat <- terra::rast(system.file("extdata", "habitat.tif", package="quadtree"))
#'
#' # create quadtree
#' qt1 <- quadtree(habitat, split_threshold = .1, adj_type = "expand")
#' plot(qt1)
#'
#' # create points at which we'll extract values
#' coords <- seq(-1000, 40010, length.out = 10)
#' pts <- cbind(coords,coords)
#'
#' # extract the cell values
#' vals <- extract(qt1, pts)
#'
#' # plot the quadtree and the points
#' plot(qt1, border_col = "gray50", border_lwd = .4)
#' points(pts, pch = 16, cex = .6)
#' text(pts, labels = round(vals, 2), pos = 4)
#'
#' # we can also extract the cell extents in addition to the values
#' extract(qt1, pts, extents = TRUE)
#' @export
setMethod("extract", signature(x = "Quadtree", y = "ANY"),
  function(x, y, extents = FALSE) {
    if (!is.matrix(y) && !is.data.frame(y))
      stop("'y' must be a matrix or a data frame")
    if (ncol(y) != 2) stop("'y' must have two columns")
    if (!is.numeric(y[, 1]) || !is.numeric(y[, 2])) stop("'y' must be numeric")
    if (extents) {
      return(x@ptr$getCellsDetails(y[, 1], y[, 2]))
    } else {
      return(x@ptr$getValues(y[, 1], y[, 2]))
    }
  }
)
