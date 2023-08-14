#' Plot probability matrix as a heatmap
#'
#' @param object A probability matrix or SpatialExperiment.
#' @param pm_cols The colnames of probability matrix. This is requires for
#' SpatialExperiment input. Assuming that the probability is stored in the colData.
#' @param targetCells Character. Optional. Can speicify one or more cells to be plotted.
#' @param n Integer. The number of randomly selected cells to be plotted.
#' This parameter will be used when targetCells is not specify.
#' @param hm_width Integer. The width of heatmap.
#' @param hm_height Integer. The height of heatmap.
#' @param clusterRows Logical. Cluster rows or not.
#' @param clusterCols Logical. Cluster columns or not.
#' @param title Title of the heatmap.
#' @param ... Ignore parameter.
#'
#' @return A ComplexHeatmap plot.
#' @export
#'
#' @docType methods
#' @name plotHoodMat
#' @rdname plotHoodMat
#' @aliases plotHoodMat plotHoodMat,matrix-method plotHoodMat,SpatialExperiment-method
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
#' plotHoodMat(spe, pm_cols = colnames(pm2))
#'
setGeneric(
  "plotHoodMat",
  function(object, ...) standardGeneric("plotHoodMat")
)

#' @rdname plotHoodMat
setMethod(
  "plotHoodMat",
  signature("matrix"),
  function(object, targetCells = NA, n = 30,
           hm_width = 4, hm_height = 15, clusterRows = TRUE, clusterCols = TRUE,
           title = "Probability of neighborhoods") {
    if (is(targetCells, "logical")) {
      targetCells <- sample(rownames(object), n)
    }

    dat <- object[targetCells, ]

    plotHoodMat_intl(dat, hm_width, hm_height, clusterRows,
                     clusterCols, title = title)
  }
)

#' @rdname plotHoodMat
setMethod(
  "plotHoodMat",
  signature("SpatialExperiment"),
  function(object, pm_cols, targetCells = NA, n = 30,
           hm_width = 4, hm_height = 15, clusterRows = TRUE, clusterCols = TRUE,
           title = "Probability of neighborhoods") {
    dat <- as.data.frame(colData(object))
    dat <- dat[, pm_cols]

    if (is(targetCells, "logical")) {
      targetCells <- sample(rownames(dat), n)
    }

    dat <- as.matrix(dat[targetCells, ])

    plotHoodMat_intl(dat, hm_width, hm_height, clusterRows,
                     clusterCols, title = title)
  }
)

plotHoodMat_intl <- function(dat, hm_width, hm_height, clusterRows,
                             clusterCols, title = title) {
  cp <- scico::scico(200, palette = "acton", direction = -1)

  ht <- ComplexHeatmap::Heatmap(dat,
    col = cp, rect_gp = grid::gpar(col = "black", lwd = 1),
    name = "Probability", column_title = title,
    column_title_gp = grid::gpar(fontsize = 15, fontface = "italic"),
    column_dend_height = unit(1, "cm"),
    row_dend_width = unit(1, "cm"), column_names_rot = 45,
    width = unit(hm_width, "cm"), height = unit(hm_height, "cm"),
    cluster_rows = clusterRows, cluster_columns = clusterCols
  )

  return(ht)
}
