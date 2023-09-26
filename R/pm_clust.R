#' Cluster the probability matrix with K-means
#'
#' @param object A probability matrix or a SpatialExperiment.
#' @param k The number of clusters. By default is 2^ncol(object)-1.
#' @param iter_max the maximum number of iterations allowed.
#' @param nstart how many random sets should be chosen.
#' @param pm_cols The colnames of probability matrix. This is requires for
#' SpatialExperiment input. Assuming that the probability is 
#' stored in the colData.
#' @param algo Algorithm to be used. Options include 
#' Hartigan-Wong, Lloyd, and MacQueen.
#' @param val_names Character. Column names used to store the clusters.
#' @param ... Ignore parameter.
#'
#' @return A probability matrix or a SpatialExperiment object. For latter,
#' the clustering results are saved in the colData of 
#' the SpatialExperiment object.
#' @export
#'
#' @docType methods
#' @name clustByHood
#' @rdname clustByHood
#' @aliases clustByHood clustByHood,matrix-method clustByHood,SpatialExperiment-method
#'
#' @examples
#'
#' m <- matrix(abs(rnorm(1000 * 100)), 1000, 100)
#'
#' clust <- clustByHood(m, k = 3)
#'
setGeneric(
  "clustByHood",
  function(object, ...) standardGeneric("clustByHood")
)


#' @rdname clustByHood
setMethod(
  "clustByHood",
  signature("matrix"),
  function(object, k = 2^ncol(object) - 1,
           iter_max = 1000, nstart = 5) {
    clust <- pmclust_intl(object, k, iter_max, nstart, algo = "Hartigan-Wong")

    return(clust)
  }
)

#' @rdname clustByHood
setMethod(
  "clustByHood",
  signature("SpatialExperiment"),
  function(object, pm_cols, k = 0,
           iter_max = 1000, nstart = 5,
           algo = "Hartigan-Wong", val_names = "clusters") {
    dat <- as.data.frame(colData(object))

    if (!all(pm_cols %in% colnames(dat))) {
      stop("The pm_cols are not included in the SpatialExperiment.")
    }

    dat <- dat[, pm_cols]

    if (k == 0) {
      k <- 2^ncol(dat) - 1
    }

    clust <- pmclust_intl(dat, k, iter_max, nstart, algo)

    colData(object)[,val_names] <- as.character(clust$cluster)
    object@metadata$centroids <- clust$centers

    return(object)
  }
)


pmclust_intl <- function(x, k, im, ns, algo) {
  clust <- stats::kmeans(
    x = x, centers = k,
    iter.max = im, nstart = ns,
    algorithm = algo
  )

  return(clust)
}
