#' Merge probability matrix into SpatialExperiment object.
#'
#' @param spe A SpatialExperiment object.
#' @param pm Probability matrix. Can be obtained by the function mergeByGroup.
#'
#' @return A SpatialExperiment object. Cell-level neighborhood information are
#' saved in the colData of the SpatialExperiment object.
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
mergeHoodSpe <- function(spe, pm) {
  if (!(all(rownames(pm) == colnames(spe)))) {
    pm <- pm[colnames(spe), ]
  }

  for (i in colnames(pm)) {
    colData(spe)[, i] <- pm[, i]
  }

  return(spe)
}
