#' Plot cells based on cell position on tissue.
#'
#' @param spe SpatialExperiment object.
#' @param targetcell Optional. Can input ONE specific cell id to zoom-in on
#' the region of a specific cell.
#' @param k_near Optional. If targetcell is specified, the k_near cells around
#' the targetcell will be plotted.
#' @param targetsize Dot size of the targetcell.
#' @param targetshape Shape of the targetcell.
#' @param targetcolor Colour of the targetcell.
#' @param scaleFactor Scale factor to align with the image.
#' @param reverseY Reverse y coordinates.
#' @param ... aesthetic mappings to pass to `ggplot2::aes_string()`.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#'
#' data("spe_test")
#'
#' plotTissue(spe, color = celltypes)
#'
plotTissue <- function(spe, targetcell = FALSE, k_near = 100, targetsize = 3,
                       targetshape = 1, targetcolor = "red",
                       scaleFactor = 1, reverseY = TRUE, ...) {
  
  if (!is(spe, "SpatialExperiment")){
    stop("The input spe must be a SpatialExperiment object.")
  }
  
  if (is.null(SpatialExperiment::spatialCoordsNames(spe))){
    stop("Coordinates are not found in the spatialCoords of spe.")
  }
  
  if (!is(targetcell, "logical")) {
    if (length(targetcell) == 1) {
      ncells <- findNearCells(spe,
        k = k_near, targetCell = targetcell, reportCellID = TRUE,
        reportDist = FALSE
      ) |>
        unlist()

      spe <- spe[, c(targetcell, ncells)]
    }
  }

  toplot <- SpatialExperiment::spatialCoords(spe) |>
    as.data.frame(optional = TRUE)

  colnames(toplot) <- c("x", "y")



  cdata <- as.data.frame(SummarizedExperiment::colData(spe), 
                         optional = TRUE)

  if ("cell_id" %in% colnames(cdata)) {
    cdata <- cdata[,!(colnames(cdata) %in% "cell_id")]
  }

  toplot <- toplot |>
    cbind(cdata) |>
    rownames2col("cell_id")

  aesmap <- rlang::enquos(...)

  # split aes params into those that are not aes i.e. static parametrisation
  if (length(aesmap) > 0) {
    is_aes <- vapply(aesmap, rlang::quo_is_symbolic, FUN.VALUE = logical(1))
    defaultmap <- lapply(aesmap[!is_aes], rlang::eval_tidy)
    aesmap <- aesmap[is_aes]
  } else {
    defaultmap <- list()
  }

  toplot[, "x"] <- scaleFactor * toplot[, "x"]
  toplot[, "y"] <- scaleFactor * toplot[, "y"]

  if (reverseY) {
    y_tmp <- toplot[, "y"]
    mid_y <- (max(y_tmp) + min(y_tmp)) / 2
    final_y <- 2 * mid_y - y_tmp
    toplot[, "y"] <- final_y
  }

  p <- ggplot2::ggplot(toplot, aes(x = x, y = y, !!!aesmap))

  p <- p +
    do.call(ggplot2::geom_point, defaultmap) +
    tissue_theme()

  if (!is(targetcell, "logical")) {
    target_df <- toplot[toplot$cell_id %in% targetcell,]

    p <- p +
      ggplot2::geom_point(data = target_df, aes(x, y), size = targetsize,
                          shape = targetshape, color = targetcolor)
  }

  return(p)
}



tissue_theme <- function(textScale = 1.1) {
  stopifnot(textScale > 0)
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      panel.grid = element_blank(),
      legend.text = element_text(size = rel(textScale)),
      legend.title = element_text(size = rel(textScale), face = "italic"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}


utils::globalVariables(c("x", "y", "cell_id"))
