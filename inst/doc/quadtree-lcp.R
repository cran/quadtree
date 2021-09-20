## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "jpeg",
  fig.height = 4.375,
  fig.width = 7
  # fig.height = 3.125,
  # fig.width = 5
)
options(rmarkdown.html_vignette.check_title = FALSE)
old_par <- par(no.readonly = TRUE)

## ---- message = FALSE, warning = FALSE----------------------------------------
library(quadtree)
library(raster)

data(habitat)
rast <- habitat
qt <- quadtree(rast, .04, split_method = "sd")

## -----------------------------------------------------------------------------
start_pt <- c(6989, 34007)
lcpf <- lcp_finder(qt, start_pt)
lcpf

## -----------------------------------------------------------------------------
start_pt <- c(6989, 34007)
end_pt <- c(33015, 38162)

# create the LCP finder object and find the LCP
lcpf <- lcp_finder(qt, start_pt)
path <- find_lcp(lcpf, end_pt)

# plot the LCP
plot(qt, crop = TRUE, na_col = NULL, border_col = "gray30", border_lwd = .3)
points(rbind(start_pt, end_pt), pch = 16, col = "red")
lines(path[, 1:2], col = "black")

## -----------------------------------------------------------------------------
lcpf

## -----------------------------------------------------------------------------
head(summarize_lcps(lcpf))

## -----------------------------------------------------------------------------
par(mfrow = c(1, 2), mar = c(.5,.5,2,.5))

plot(qt, crop = TRUE, na_col = NULL, border_col = "gray30", border_lwd = .3,
     legend = FALSE, axes = FALSE,
     main = "cells to which LCPs have been found")
points(lcpf, pch=16, cex=.3)
points(rbind(start_pt, end_pt), pch = 16, col = "red")

plot(qt, crop = TRUE, na_col = NULL, border_col = "gray30", border_lwd = .3,
     legend = FALSE, axes = FALSE, main = "LCPs calculated")
lines(lcpf)
points(rbind(start_pt, end_pt), pch = 16, col = "red")

## -----------------------------------------------------------------------------
start_pt <- c(16715, 25634)
lcpf <- lcp_finder(qt, start_pt)
paths <- find_lcps(lcpf)

plot(qt, crop = TRUE, na_col = NULL, border_col = "gray30", border_lwd = .3,
     main = "All LCPs from a given start point")
lines(lcpf, lwd = .5)
points(start_pt[1], start_pt[2], col = "black", bg = "red", pch = 21, cex = 1.2)

## -----------------------------------------------------------------------------
start_pt <- c(16715, 25634)

dist <- 6500
xlim <- c(start_pt[1] - dist, start_pt[1] + dist)
ylim <- c(start_pt[2] - dist, start_pt[2] + dist)

lcpf1 <- lcp_finder(qt, start_pt, xlim, ylim, search_by_centroid = FALSE)
paths1 <- find_lcps(lcpf1) 

lcpf2 <- lcp_finder(qt, start_pt, xlim, ylim, search_by_centroid = TRUE)
paths2 <- find_lcps(lcpf2)

## -----------------------------------------------------------------------------
par(mfrow = c(1, 2))
plot(qt, na_col = NULL, border_lwd = .3, legend = FALSE, xlim = xlim,
     ylim = ylim, main = "search_by_centroid = FALSE")
rect(xlim[1], ylim[1], xlim[2], ylim[2], border = "red", lwd = 2)
lines(lcpf1)
points(lcpf1, pch=16, cex=.4)
points(start_pt[1], start_pt[2], col = "black", bg = "red", pch = 21, cex = 1.2)

plot(qt, na_col = NULL, border_lwd = .3, legend = FALSE, xlim = xlim,
     ylim = ylim, main = "search_by_centroid = TRUE")
rect(xlim[1], ylim[1], xlim[2], ylim[2], border = "red", lwd = 2)
lines(lcpf2)
points(lcpf2, pch=16, cex=.4)
points(start_pt[1], start_pt[2], col = "black", bg = "red", pch = 21, cex = 1.2)

## -----------------------------------------------------------------------------
lcpf <- lcp_finder(qt, start_pt)
paths <- find_lcps(lcpf, limit = 3000)

plot(qt, crop = TRUE, na_col = NULL, border_col = "gray30", border_lwd = .3,
     main = "All LCPs with cost-distance less than 3000")
lines(lcpf, lwd = .5)
points(start_pt[1], start_pt[2], col = "red", pch = 16)

## -----------------------------------------------------------------------------
start_pt <- c(16715, 25634)

lcpf <- lcp_finder(qt, start_pt)
paths1 <- find_lcps(lcpf, limit = 3000)
nrow(paths1)

paths2 <- find_lcps(lcpf, limit = 1000)
nrow(paths2) # same as for paths1!!!

## ---- echo=FALSE--------------------------------------------------------------
par(old_par)

