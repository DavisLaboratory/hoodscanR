#' Find the k-th nearest cells for each cell
#'
#' @param dat A SpatialExperiment object, can be generated using function `readHoodData`.
#' @param k The maximum number of nearest cells to compute.
#' @param targetCell Specify the cells to be the target cell for finding nearest cells.
#' @param reportCellID Logical. Set to TRUE to report cell id instead of cell types.
#' @param reportDist Logical. Set to TRUE to report the distance matrix.
#' @param anno_col Character vector. The name of annotation column to use.
#'
#' @return A list includes a data.frame and a matrix, describing the cell types
#' and distances of the k-th nearest cells of each cell.
#' @details The `findNearCells` function uses the `nn2` function from the `RANN` package,
#' which uses the Approximate Near Neighbor (ANN) C++ library. For more infromation on the
#' ANN library please see http://www.cs.umd.edu/~mount/ANN/.
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
findNearCells <- function(dat, k = 100, targetCell = FALSE,
                          reportCellID = FALSE,
                          reportDist = TRUE, anno_col = 0) {
  
  if (!is(dat, "SpatialExperiment")) {
    stop("Please use SpatialExperiment as input.")
  }
  
  if (is.null(SpatialExperiment::spatialCoordsNames(dat))){
    stop("Coordinates are not found in the spatialCoords of spe.")
  }

  if (is.null(rownames(SpatialExperiment::spatialCoords(dat)))){
    rownames(SpatialExperiment::spatialCoords(dat)) <- colnames(dat)
  }
    
  cell_pos_dat <- SpatialExperiment::spatialCoords(dat)
  colnames(cell_pos_dat) <- c("x", "y")

  if (anno_col == 0) {
    anno_col <- "cell_annotation"
  }

  cell_annotation <- SummarizedExperiment::colData(dat)[, anno_col]

  query <- targetCell

  if (isFALSE(targetCell)) {
    query <- cell_pos_dat
  } else {
    if (length(targetCell) == 1) {
      query <- t(as.matrix(cell_pos_dat[targetCell, ]))
      rownames(query) <- targetCell
    } else {
      query <- cell_pos_dat[targetCell, ]
    }
  }

  searchcells <- RANN::nn2(data = cell_pos_dat, query = query,
                           k = k + 1, searchtype = "priority")

  closest <- searchcells[[1]]

  idxcell <- closest[, 1]
  idxclosecell <- closest[, seq(from = 2, to = ncol(closest))]

  if (isFALSE(reportCellID)) {
    idxcell[] <- rownames(cell_pos_dat)
    idxclosecell[] <- cell_annotation[c(idxclosecell)]
  } else {
    idxcell[] <- rownames(cell_pos_dat)[c(idxcell)]
    idxclosecell[] <- rownames(cell_pos_dat)[c(idxclosecell)]
  }

  if (!isFALSE(targetCell) & length(targetCell) == 1) {
    result <- as.data.frame(t(as.data.frame(idxclosecell)))
    rownames(result) <- targetCell
  } else {
    result <- cbind(idxcell, idxclosecell) |>
      as.data.frame() |>
      col2rownames("idxcell")
  }

  colnames(result) <- paste0("nearest_cell_", seq(k))

  if (isTRUE(reportDist)) {
    dist <- searchcells$nn.dists[, seq(from = 2, to = (k + 1))]
    rownames(dist) <- rownames(result)
    colnames(dist) <- colnames(result)
    result <- list(result, dist)
    names(result) <- c("cells", "distance")
  }

  return(result)
}
