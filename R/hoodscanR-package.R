#' @importFrom SummarizedExperiment colData
#' @import SpatialExperiment
#' @importFrom dplyr across
#' @import ggplot2
#' @importFrom SummarizedExperiment assay
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr group_by
#' @importFrom S4Vectors metadata
#' @importFrom S4Vectors metadata<-
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr where
#' @importFrom methods is
#' @importFrom grid gpar
#' @importFrom grid unit
#' @importFrom stats cor
#' @importFrom stats median
#' @import Rcpp
#' @import knitr
#' @importFrom utils globalVariables
#' @importFrom rmarkdown draft
NULL

#' Method to identify cellular spatial neighbourhood from single cell spatial
#' transcriptomics data.
#'
#' `hoodscanR` implements a novel method to scan for cell neighbourhood from
#' spatial transcriptomics data at single cell level, such as CosMx and MERFISH etc.
#' `hoodscanR` takes the cellular position and cell type annotations
#' as inputs, allowing cellular spatial neighbourhood analysis.
#'
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
NULL


#' Example test data
#'
#' hoodscanR-package has 1 datasets: \itemize{
#'   \item spe_test  An example data
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
