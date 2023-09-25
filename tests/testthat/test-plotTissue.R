test_that("testing plotTissue", {
  data("spe_test")
  
  spe <- readHoodData(spe, anno_col = "celltypes")

  expect_silent(plotTissue(spe, color = celltypes))
  
  expect_error(plotTissue(spe, targetcell = "xyz"))
  
  expect_silent(plotTissue(spe, targetcell = "Lung9_Rep1_5_7"))
  
  expect_silent(plotTissue(spe, targetcell = "Lung9_Rep1_5_7",
                           k_near = 100))
  
  expect_error(plotTissue(spe, targetcell = "Lung9_Rep1_5_7",
                          k_near = "xx"))
})
