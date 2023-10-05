#' Merge probability matrix into SpatialExperiment object.
#'
#' @param spe A SpatialExperiment object.
#' @param pm Probability matrix. Can be obtained by the function mergeByGroup.
#' @param val_names Character vector with length of the ncol of pm.
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
mergeHoodSpe <- function(spe, pm, val_names = NULL) {
  
  if (!(all(rownames(pm) == colnames(spe)))) {
    pm <- pm[colnames(spe), ]
  }
  
  if(!is.null(val_names)){
    if(length(val_names) == ncol(pm)){
      colnames(pm) <- val_names
    } else {
      message("The length of val_names is not right, 
              using colnames(pm) instead.")
    }
  }
  
  colData(spe)[,colnames(pm)] <- pm

  return(spe)
}
