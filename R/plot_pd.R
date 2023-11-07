#' Plot probability distribution
#'
#' @param object A probability matrix or SpatialExperiment.
#' @param targetCells Character. Optional. Can speicify one or more cells to be plotted.
#' @param pm_cols The colnames of probability matrix. This is requires for
#' SpatialExperiment input. Assuming that the probability is stored in the colData.
#' @param by_cluster Logical. By default is TRUE, to plot distribution by each cluster.
#' @param show_clusters Character. The cluster to be ploted, by default is 1 to 6.
#' @param plot_all Logical. By default is FALSE, set this to true to plot box
#' plot instead of bar plot to show all cells in each cluster.
#' @param sample_size Integer. By default is 2, sampling two cell from each
#' cluster to be plotted.
#' @param ... aesthetic mappings to pass to `ggplot2::aes_string()`.
#'
#' @return A ggplot object.
#' @export
#'
#' @docType methods
#' @name plotProbDist
#' @rdname plotProbDist
#' @aliases plotProbDist plotProbDist,matrix-method plotProbDist,SpatialExperiment-method
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
#' plotProbDist(spe, pm_cols = colnames(pm2))
#'
setGeneric(
  "plotProbDist",
  function(object, ...) standardGeneric("plotProbDist")
)


#' @rdname plotProbDist
setMethod(
  "plotProbDist",
  signature("matrix"),
  function(object, targetCells = NA, ...) {
    if (is(targetCells, "logical")) {
      targetCells <- rownames(object)[seq(6)]
    }
    
    dat <- format_dat(object, targetCells = targetCells)

    p <- plotProbDist_intl(dat, ...) +
      facet_wrap(~cells, ncol = 4)

    return(p)
  }
)

#' @rdname plotProbDist
setMethod(
  "plotProbDist",
  signature("SpatialExperiment"),
  function(object, pm_cols, targetCells = NA, by_cluster = FALSE,
           show_clusters = as.character(seq(6)), plot_all = FALSE,
           sample_size = 2, ...) {
    dat <- as.data.frame(colData(object), optional = TRUE)

    if (!all(pm_cols %in% colnames(dat))) {
      stop("The pm_cols are not included in the SpatialExperiment.")
    }

    if (isTRUE(by_cluster)) {
      if (!("clusters" %in% colnames(dat))) {
        stop("Cannot find the clusters column in the SpatialExperiment, check column names.")
      }

      if (all(!(show_clusters %in% dat[, "clusters"]))) {
        stop("Cannot find the show_clusters value in the clusters column.")
      }

      dat <- dat[, c(pm_cols, "clusters")]

      dat <- as.data.frame(dat, optional = TRUE) |>
        rownames2col("cells")
      
      dat <- dat[dat$clusters %in% show_clusters,]

      if (isTRUE(plot_all)) {
        p <- plotProbDist_box_intl(dat, pm_cols, ...) +
          facet_wrap(~clusters)
      } else {
        
        datx <- sample_rows(dat, "clusters", sample_size)
        
        datx <- format_dat_pivot(datx, cols2ex = c("cells","clusters"))

        p <- plotProbDist_intl(datx, ...) +
          facet_wrap(~ clusters + cells, ncol = 4)
      }
    } else {
      dat <- dat[, c(pm_cols)]

      if (is(targetCells, "logical")) {
        targetCells <- rownames(dat)[seq(6)]
      }

      dat <- format_dat(dat, targetCells = targetCells)

      p <- plotProbDist_intl(dat, ...) +
        facet_wrap(~cells, ncol = 4)
    }

    return(p)
  }
)



sample_rows <- function(df, group_var, sample_size) {
  grouped <- split(df, df[[group_var]])
  
  sampled <- lapply(grouped, function(group_df) {
    if (nrow(group_df) >= sample_size) {
      group_df[sample(seq_len(nrow(group_df)), size = sample_size), ]
    } else {
      group_df
    }
  })
  
  sampled_df <- do.call(rbind, sampled)
  rownames(sampled_df) <- NULL
  return(sampled_df)
}


format_dat <- function(object, targetCells = NULL, cols2ex = "cells"){
  
  dat <- as.data.frame(object, optional = TRUE) |>
    rownames2col("cells")
  if (is.null(targetCells)){
    filtered_dat <- dat
  } else {
    dat <- as.data.frame(object, optional = TRUE) |>
      rownames2col("cells")
    filtered_dat <- dat[dat$cells %in% targetCells, ]
  }
  
  reshaped_dat <- format_dat_pivot(filtered_dat, cols2ex)
  return(reshaped_dat)
}

format_dat_pivot <- function(x, cols2ex = "cells"){
  
  reshaped_dat <- reshape(x, varying = names(x)[!(names(x) %in% cols2ex)],
                          v.names = "probability", timevar = "hoods",
                          times = names(x)[!(names(x) %in% cols2ex)],
                          direction = "long")
  reshaped_dat <- reshaped_dat[,names(reshaped_dat) != "id"]
  reshaped_dat$cells <- factor(reshaped_dat$cells, levels = x$cells)
  dat <- reshaped_dat[order(reshaped_dat$cells, reshaped_dat$hoods),]
  rownames(dat) <- NULL
  
  return(dat)
}


plotProbDist_intl <- function(x, ...) {
  aesmap <- rlang::enquos(...)

  # split aes params into those that are not aes i.e. static parametrisation
  if (length(aesmap) > 0) {
    is_aes <- vapply(aesmap, rlang::quo_is_symbolic, FUN.VALUE = logical(1))
    defaultmap <- lapply(aesmap[!is_aes], rlang::eval_tidy)
    aesmap <- aesmap[is_aes]
  } else {
    defaultmap <- list()
  }

  defaultmap[["stat"]] <- "identity"

  p <- ggplot2::ggplot(x, aes(x = hoods, y = probability, !!!aesmap))

  p <- p +
    do.call(ggplot2::geom_bar, defaultmap) +
    pm_theme()

  return(p)
}

plotProbDist_box_intl <- function(x, pm_cols, ...) {
  aesmap <- rlang::enquos(...)

  # split aes params into those that are not aes i.e. static parametrisation
  if (length(aesmap) > 0) {
    is_aes <- vapply(aesmap, rlang::quo_is_symbolic, FUN.VALUE = logical(1))
    defaultmap <- lapply(aesmap[!is_aes], rlang::eval_tidy)
    aesmap <- aesmap[is_aes]
  } else {
    defaultmap <- list()
  }
  
  x <- format_dat_pivot(x, cols2ex = c("cells","clusters"))

  p <- ggplot2::ggplot(x, aes(x = hoods, y = probability, !!!aesmap))

  p <- p +
    do.call(ggplot2::geom_boxplot, defaultmap) +
    pm_theme()

  return(p)
}


pm_theme <- function(textScale = 1.1) {
  stopifnot(textScale > 0)
  ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      panel.grid = element_blank(),
      legend.text = element_text(size = rel(textScale)),
      legend.title = element_text(size = rel(textScale), face = "italic"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
}

utils::globalVariables(c("cells", "clusters", "hoods", "probability"))
