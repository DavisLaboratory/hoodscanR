test_that("testing plotTissue", {
  data("spe_test")

  expect_silent(plotTissue(spe, color = celltypes))
})
