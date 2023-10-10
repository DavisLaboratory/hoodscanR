#' Calculate metrics for probability matrix
#'
#' @param spe A SpatialExperiment object.
#' @param pm Optional. The probability matrix.
#' @param pm_cols The colnames of probability matrix. This is requires for
#' SpatialExperiment input. Assuming that the probability is 
#' stored in the colData.
#' @param val_names Character vector with length of 2. Column names
#' used to store calculated entropy and perplexity.
#'
#' @return A SpatialExperiment object. Calculated entropy and perplexity are
#' saved as columns in the colData of the SpatialExperiment object.
#' Entropy and perplexity are calculated based on information theory:
#' 
#' P(x) is the probability calculated from the scanHoods function.
#' 
#' Entropy H(x) = -P(x)log2(P(x))
#' 
#' Perplexity P(x) = 2^H(x)
#' 
#' By default, the calculated entropy and perplexity will be stored
#' in the colData of the input spe, with column name as entropy and
#' perplexity.
#' 
#' @export
#'
#' @examples
#'
#' data("spe_test")
#'
#' spe <- readHoodData(spe, anno_col = "celltypes")
#'
#' fnc <- findNearCells(spe, k = 100)
#'
#' pm <- scanHoods(fnc$distance)
#'
#' pm2 <- mergeByGroup(pm, fnc$cells)
#'
#' spe <- mergeHoodSpe(spe, pm2)
#'
#' spe <- calcMetrics(spe, pm_cols = colnames(pm2))
#'
calcMetrics <- function(spe, pm = NA, pm_cols = NA,
                        val_names = c("entropy","perplexity")) {
  if (!is(spe, "SpatialExperiment")){
    stop("The input spe must be a SpatialExperiment object.")
  }
  if (is(pm, "logical")) {
    if (is(pm_cols, "logical")) {
      stop("Need to input either the pm or pm_cols parameters.")
    } else {
      pm <- as.data.frame(colData(spe), 
                          optional = TRUE)[, pm_cols] |>
        as.matrix()
    }
  } else {
    pm <- pm
  }

  result <- calculate_metrics(pm)
  
  colData(spe)[, val_names] <- result

  return(spe)
}
