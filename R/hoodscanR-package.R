#' @importFrom SummarizedExperiment colData
#' @import SpatialExperiment
#' @import ggplot2
#' @importFrom SummarizedExperiment assay
#' @importFrom methods is
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom stats cor
#' @importFrom stats median
#' @import Rcpp
#' @import knitr
#' @importFrom utils globalVariables
#' @importFrom rmarkdown draft
#' @importFrom stats aggregate 
#' @importFrom stats reshape
NULL

#' Method to identify cellular spatial neighbourhood from single cell spatial
#' transcriptomics data.
#'
#' `hoodscanR` implements a novel method to scan for cell neighbourhood from
#' spatial transcriptomics data at single cell level, such as CosMx and MERFISH etc.
#' `hoodscanR` takes the cellular position and cell type annotations
#' as inputs, allowing cellular spatial neighbourhood analysis.
#'
#' Key neighborhood analysis functions include  \code{\link{findNearCells}, 
#' \link{scanHoods}, \link{mergeByGroup}, \link{calcMetrics}, 
#' \link{clustByHood}}. 
#' 
#' Key visualisation functions include 
#' \code{\link{plotTissue}, \link{plotHoodMat}, \link{plotColocal},
#' \link{plotProbDist}}.
#'
#'
#' @author Ning Liu \email{liu.n@@wehi.edu.au}
#' @name hoodscanR-package
#' @docType package
#' @aliases hoodscanR hoodscanR-package
#' @keywords internal
#'
#' @useDynLib hoodscanR
#'
"_PACKAGE"


#' Example test spatial transcriptomics data
#'
#' hoodscanR-package has 1 datasets: \itemize{
#'   \item spe_test  Example test spatial transcriptomics data in
#'  SpatialExperiment format. This test data is randomly subsetting 
#'  from the publicly available CosMx non-small cell lung cancer data.
#'  Source data: https://nanostring.com/products/cosmx-spatial-molecular-imager/nsclc-ffpe-dataset/.
#'  }
#'
#' @docType data
#' @name spe_test
#' @usage data("spe_test")
#' @return A SpatialExperiment object
#' @keywords internal
#' @format A SpatialExperiment object
#' @examples
#' data(spe_test)
"spe"
