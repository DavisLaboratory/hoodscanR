test_that("Testing readHoodData", {
  data("spe_test")

  spe1 <- readHoodData(spe, anno_col = "celltypes")

  expect_true("cell_annotation" %in% colnames(colData(spe1)))

  expect_error(readHoodData(spe, anno_col = "xxx"))
  
  cell_pos_dat <- SpatialExperiment::spatialCoords(spe) |>
    as.data.frame() |>
    rownames2col("cell_id")
  cell_pos_dat <- cell_pos_dat[,c("cell_id","x","y")]
  
  cell_anno_dat <- SummarizedExperiment::colData(spe) |>
    as.data.frame() |>
    rownames2col("cell_id")
  cell_anno_dat <-  cell_anno_dat[,c("cell_id","celltypes")]
  
  spe2 <- readHoodData(cell_anno_dat = cell_anno_dat, 
                       cell_pos_dat = cell_pos_dat)
  
  expect_equal(ncol(spe1), ncol(spe2))
  
  expect_true("cell_annotation" %in% names(colData(spe2)))

})
