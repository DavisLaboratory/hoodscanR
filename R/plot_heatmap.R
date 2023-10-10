#' Plot heatmap for neighbourhood analysis
#'
#' @param object A probability matrix or SpatialExperiment.
#' @param pm_cols The colnames of probability matrix. This is requires for
#' SpatialExperiment input. Assuming that the probability is stored in the colData.
#' @param self_cor Logical. By default is TRUE, inidicating running a correlation
#' between neighbourhoods to perform a simple co-localization analysis.
#' When this set to FALSE, it will plot the average probability of each
#' neighbourhood by group using the by_group parameter.
#' @param by_group Character. This is required when self_cor is set to FALSE.
#' @param hm_width Integer. The width of heatmap.
#' @param hm_height Integer. The height of heatmap.
#' @param cluster_row Logical. Cluster rows.
#' @param cluster_col Logical. Cluster columns.
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
#' @aliases plotColocal plotColocal,matrix-method plotColocal,SpatialExperiment-method
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
#' plotColocal(spe, pm_cols = colnames(pm2), self_cor = FALSE, by_group = "cell_annotation")
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
  function(object, pm_cols, self_cor = TRUE, by_group = NULL, hm_width = 5,
           hm_height = 5, cluster_row = TRUE, cluster_col = TRUE,
           return_matrix = FALSE) {
    dat <- as.data.frame(colData(object), optional = TRUE)

    if (!all(pm_cols %in% colnames(dat))) {
      stop("The pm_cols are not included in the SpatialExperiment.")
    }

    if (self_cor) {
      cor.m <- cor(dat[, pm_cols])
      ht <- plotColocal_intl(cor.m,
        hm_width = hm_width, hm_height = hm_height,
        cluster_row = cluster_row, cluster_col = cluster_col
      )
    } else {
      if (is.null(by_group) | length(by_group) != 1 | !(by_group %in% colnames(dat))) {
        stop("by_group should be the columns in colData")
      } else {
        datx <- dat[, c(pm_cols, by_group)]
        
        cor.m <- mean_by_group(datx, by_group) |>
          as.data.frame(optional = TRUE) |>
          col2rownames(by_group) |>
          as.matrix()
        ht <- plotColocal_intl(cor.m,
          title = "Mean probability within groups", legend.name = "Prob.",
          hm_width = hm_width, hm_height = hm_height, self_cor = FALSE
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
  df[[group_col]] <- as.character(df[[group_col]])
  
  numeric_cols <- vapply(df, is.numeric, logical(1))
  
  grouped <- aggregate(df[, numeric_cols], by = list(df[[group_col]]), FUN = mean, na.rm = TRUE)
  
  colnames(grouped)[1] <- group_col
  
  return(grouped)
}


plotColocal_intl <- function(m, col.pal = NA,
                             title = "Pearson correlation between neighbourhoods",
                             title.size = 15, legend.name = "Cor.",
                             hm_width, hm_height, self_cor = TRUE,
                             cluster_row = FALSE, cluster_col = FALSE) {
  if (isTRUE(self_cor)) {
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
    cluster_rows = cluster_row, cluster_columns = cluster_col
  )

  return(ht)
}
