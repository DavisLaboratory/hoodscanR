test_that("testing plotTissue", {
  data("spe_test")
  
  spe <- readHoodData(spe, anno_col = "celltypes")
  
  p <- plotTissue(spe, color = cell_annotation)
  expect_s3_class(p, "ggplot")
  
  expect_error(plotTissue(spe, targetcell = "xyz"))
  
  p <- plotTissue(spe, targetcell = "Lung9_Rep1_5_7")
  expect_s3_class(p, "ggplot")
  
  p <- plotTissue(spe, targetcell = "Lung9_Rep1_5_7",
                  k_near = 100)
  expect_s3_class(p, "ggplot")
  
  expect_error(plotTissue(spe, targetcell = "Lung9_Rep1_5_7",
                          k_near = "xx"))
})
