#' Plot heatmap for neighbourhood analysis
#'
#' @param object A probability matrix or SpatialExperiment.
#' @param pm_cols The colnames of probability matrix. This is requires for
#' SpatialExperiment input. Assuming that the probability is stored in the colData.
#' @param self.cor Logical. By default is TRUE, inidicating running a correlation
#' between neighbourhoods to perform a simple co-localization analysis.
#' When this set to FALSE, it will plot the average probability of each
#' neighbourhood by group using the byGroup parameter.
#' @param byGroup Character. This is required when self.cor is set to FALSE.
#' @param hm_width Integer. The width of heatmap.
#' @param hm_height Integer. The height of heatmap.
#' @param clusterRows Logical. Cluster rows.
#' @param clusterCols Logical. Cluster columns.
#' @param return_matrix Logical. Export a numeric matrix .
#' @param ... Ignore parameter.
#'
#' @return A ComplexHeatmap plot. When return_matrix is set to TRUE,
#' return a matrix Object.
#' @export
#'
#'
#' @docType methods
#' @rdname plotColocal
#'
#' @aliases plotColocal plotColocal, matrix-method
#' @aliases plotColocal, SpatialExperiment-method plotColocal
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
#' plotColocal(spe, pm_cols = colnames(pm2))
#'
setGeneric(
  "plotColocal",
  function(object, ...) standardGeneric("plotColocal")
)


#' @rdname plotColocal
setMethod(
  "plotColocal",
  signature("matrix"),
  function(object, hm_width = 5, hm_height = 5) {
    cor.m <- cor(object)
    ht <- plotColocal_intl(cor.m, hm_width = hm_width, hm_height = hm_height)

    ComplexHeatmap::draw(ht)
  }
)

#' @rdname plotColocal
setMethod(
  "plotColocal",
  signature("SpatialExperiment"),
  function(object, pm_cols, self.cor = TRUE, byGroup = NA, hm_width = 5,
           hm_height = 5, clusterRows = TRUE, clusterCols = TRUE,
           return_matrix = FALSE) {
    dat <- as.data.frame(colData(object))

    if (!all(pm_cols %in% colnames(dat))) {
      stop("The pm_cols are not included in the SpatialExperiment.")
    }

    if (self.cor) {
      cor.m <- cor(dat[, pm_cols])
      ht <- plotColocal_intl(cor.m,
        hm_width = hm_width, hm_height = hm_height,
        clusterRows = clusterRows, clusterCols = clusterCols
      )
    } else {
      if (is(byGroup, "logical") | length(byGroup) != 1 | !(byGroup %in% colnames(dat))) {
        stop("byGroup should be the columns in colData")
      } else {
        datx <- dat[, c(pm_cols, byGroup)]
        cor.m <- mean_by_group(datx, byGroup) |>
          as.data.frame() |>
          column_to_rownames(byGroup) |>
          as.matrix()
        ht <- plotColocal_intl(cor.m,
          title = "Mean probability within groups", legend.name = "Prob.",
          hm_width = hm_width, hm_height = hm_height, self.cor = FALSE
        )
      }
    }

    if (isTRUE(return_matrix)) {
      return(cor.m)
    } else {
      ComplexHeatmap::draw(ht)
    }
  }
)

mean_by_group <- function(df, group_col) {
  df[, group_col] <- as.character(df[, group_col])

  df_out <- df |>
    group_by(!!rlang::sym(group_col)) |>
    summarise(across(where(is.numeric), mean))

  return(df_out)
}

plotColocal_intl <- function(m, col.pal = NA,
                             title = "Pearson correlation between neighbourhoods",
                             title.size = 15, legend.name = "Cor.",
                             hm_width, hm_height, self.cor = TRUE,
                             clusterRows = FALSE, clusterCols = FALSE) {
  if (isTRUE(self.cor)) {
    if (is(col.pal, "logical")) {
      cp <- circlize::colorRamp2(seq(-1, 1, by = 0.01),
                                 scico::scico(201, palette = "roma", direction = -1))
    } else {
      cp <- col.pal
    }
  } else {
    if (is(col.pal, "logical")) {
      cp <- circlize::colorRamp2(seq(0, 1, by = 0.01),
                                 scico::scico(101, palette = "lajolla"))
    } else {
      cp <- col.pal
    }
  }



  ht <- ComplexHeatmap::Heatmap(m,
    col = cp, rect_gp = gpar(col = "white", lwd = 2),
    name = legend.name, column_title = title,
    column_title_gp = gpar(fontsize = title.size, fontface = "italic"),
    column_dend_height = unit(1, "cm"),
    row_dend_width = unit(1, "cm"), column_names_rot = 45,
    width = unit(hm_width, "cm"), height = unit(hm_height, "cm"),
    cluster_rows = clusterRows, cluster_columns = clusterCols
  )

  return(ht)
}
