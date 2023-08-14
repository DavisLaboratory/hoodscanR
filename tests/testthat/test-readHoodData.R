test_that("Testing readHoodData", {
  data("spe_test")

  spe1 <- readHoodData(spe, anno_col = "celltypes")

  expect_true("cell_annotation" %in% colnames(colData(spe1)))

  expect_error(readHoodData(spe, anno_col = "xxx"))

})
