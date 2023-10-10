#' Read cellular position and annotation data into a list object.
#'
#' @param spe SpatialExperiment object.
#' @param anno_col Character. The column name of the annotation to be used in
#' the following neighbourhood analysis.
#' @param cell_pos_dat data.frame object contains the cellular positions.
#' @param cell_anno_dat data.frame object contains the cell annotations.
#' @param pos_col Character. If the x and y are in the colData instead of in the
#' SpatialCoords of spe, can specify this parameter.
#'
#' @return A SpatialExperiment object.
#'
#' @export
#'
#' @examples
#' data("spe_test")
#'
#' spe <- readHoodData(spe, anno_col = "celltypes")
#'
readHoodData <- function(spe = NA, anno_col = NA,
                         cell_pos_dat = NA, cell_anno_dat = NA,
                         pos_col = NA) {
  if (is(spe, "logical") & is(anno_col, "logical")) {
    if (is(cell_pos_dat, "logical") | is(cell_anno_dat, "logical")) {
      stop("You need to input either a SummarizedExperiment object with parameter
           spe or two dataframes with parameters cell_pos_dat and cell_anno_dat")
    } else if (!is.data.frame(cell_pos_dat) | !is.data.frame(cell_anno_dat)) {
      stop("cell_pos_dat and cell_anno_dat should be data.frame objects.")
    }
    if (ncol(cell_pos_dat) != 3) {
      stop("The cell_pos_dat is expected to have three columns. cell_id, x, and y.")
    }
    if (ncol(cell_anno_dat) != 2) {
      stop("The cell_pos_dat is expected to have two columns.
           cell_id and annotations.")
    }
    if (nrow(cell_pos_dat) != nrow(cell_anno_dat)) {
      stop("The cell_pos_dat should have the same amount of cells as cell_anno_data.")
    }
  } else if (!is(spe, "SummarizedExperiment") | !is(anno_col, "character")) {
    stop("spe should be a SummarizedExperiment object and anno_col should be
         a column name in the colData of spe.")
  }

  if (is(spe, "logical")) {
    colnames(cell_pos_dat) <- c("cell_id", "x", "y")
    colnames(cell_anno_dat) <- c("cell_id", "cell_annotation")

    dat <- list(cell_pos_dat, cell_anno_dat)
    names(dat) <- c("cell_position", "cell_annotation")


    dummy_exp_m <- matrix(1, nrow = 10, ncol = nrow(cell_pos_dat))
    colnames(dummy_exp_m) <- cell_pos_dat$cell_id
    rownames(dummy_exp_m) <- paste0("gene_", seq(10))
    spe_n <- SpatialExperiment::SpatialExperiment(
      assay = list("counts" = as.data.frame(dummy_exp_m, 
                                            optional = TRUE)),
      colData = cell_anno_dat |> col2rownames("cell_id"),
      spatialCoords = cell_pos_dat |> col2rownames("cell_id") |>
        as.matrix(),
      metadata = list("dummy" = 1)
    )
  } else { # if input is spe
    if (!(anno_col %in% colnames(colData(spe)))) {
      stop("anno_col is not in the colData of spe.")
    }

    col_dat <- as.data.frame(colData(spe), optional = TRUE)
    colnames(col_dat)[colnames(col_dat) == anno_col] <- "cell_annotation"

    if (is(spe, "SpatialExperiment")) {
      
      if (is.null(SpatialExperiment::spatialCoordsNames(spe))){
        stop("Coordinates are not found in the spatialCoords of spe.")
      }
      
      if (is(pos_col, "logical")) {
        pos_dat <- spatialCoords(spe)
        colnames(pos_dat) <- c("x", "y")
      } else {
        if (!all(pos_col %in% colnames(colData(spe)))) {
          stop("pos_col is not in the colData of spe.")
        }
        pos_dat <- as.data.frame(colData(spe), 
                                 optional = TRUE)[, pos_col]
        colnames(pos_dat) <- c("x", "y")
        pos_dat <- as.matrix(pos_dat)
      }
    } else {
      if (is(pos_col, "logical") | length(pos_col) != 2) {
        stop("You need to specify 2 cell position columns in colData of the spe
             with parameter pos_col.")
      }

      if (!all(pos_col %in% colnames(colData(spe)))) {
        stop("pos_col is not in the colData of spe.")
      }
      pos_dat <- as.data.frame(colData(spe), 
                               optional = TRUE)[, pos_col]
      colnames(pos_dat) <- c("x", "y")
      pos_dat <- as.matrix(pos_dat)
    }

    spe_n <- SpatialExperiment::SpatialExperiment(
      assay = list("counts" = as.data.frame(as.matrix(assay(spe, "counts")),
                                            optional = TRUE)),
      colData = col_dat,
      spatialCoords = pos_dat,
      metadata = list("dummy" = 0)
    )
  }

  return(spe_n)
}
