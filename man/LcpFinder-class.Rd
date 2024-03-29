% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{LcpFinder-class}
\alias{LcpFinder-class}
\alias{LcpFinder}
\title{LcpFinder Class}
\description{
This S4 class is a wrapper around a \code{CppLcpFinder} C++ object
that is made available to R via the 'Rcpp' package. Instances of this class
can be created from a \code{\link{Quadtree}} object using the
\code{\link{lcp_finder}} function.

The methods of the C++ object (\code{\link{CppLcpFinder}}) can be
accessed from R, but the typical end-user should have no need of these
methods - they are meant for internal use. That being said, descriptions of
the available methods can be found on the \code{\link{CppLcpFinder}}
documentation page.
}
\details{
Functions for creating a \code{LcpFinder} object: \itemize{
  \item \code{\link{lcp_finder}()}
}
Methods: \itemize{
  \item \code{\link{find_lcp}()}
  \item \code{\link{find_lcps}()}
  \item \code{\link[=plot.LcpFinder]{plot}()}
  \item \code{\link[=show.LcpFinder]{show}()}
  \item \code{\link{summarize_lcps}()}
  \item \code{\link[=summary.LcpFinder]{summary}()}
}
}
\section{Slots}{

\describe{
\item{\code{ptr}}{a C++ object of class \code{CppLcpFinder}}
}}

