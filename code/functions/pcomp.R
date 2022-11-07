#' File src/library/stats/R/prcomp.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
#' 
#' @title: PCA
#' @version: 0.1.0 /11/05/2022
#'
#' @source source("functions/pcomp.R")
#'
#' @example pcomp(iris %>% select(1:4), center_ = TRUE, scale_ = TRUE)
#' 

pcomp <-
  function(x, retx = TRUE, center_ = TRUE, scale_ = TRUE, tol = NULL, ...)
  {
    x <- as.matrix(x)
    x <- scale(x, center = center_, scale = scale_)
    s <- svd(x, nu = 0)
    cen <- attr(x, "scaled:center")
    sc <- attr(x, "scaled:scale")
    s$d <- s$d / sqrt(max(1, nrow(x) - 1))
    if (!is.null(tol)) {
      # get rank at least one even for a 0 matrix ----
      rank <- sum(s$d > (s$d[1L]*tol))
      if (rank < ncol(x)) {
        s$v <- s$v[, 1L:rank, drop = FALSE]
        s$d <- s$d[1L:rank]
      }
    }
    dimnames(s$v) <-
      list(colnames(x), paste0("PC", seq_len(ncol(s$v))))
    r <- list(sdev = s$d, rotation = s$v,
              center = if(is.null(cen)) FALSE else cen,
              scale = if(is.null(sc)) FALSE else sc)
    # if return the rotated var ----
    if (retx) r$x <- x %*% s$v
    r
  }