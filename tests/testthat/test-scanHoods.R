test_that("Testing scanHoods", {
  m <- matrix(abs(rnorm(1000 * 100)), 1000, 100)

  pm <- scanHoods(m)

  expect_equal(nrow(pm),1000)

  expect_equal(ncol(pm), 100)

  expect_equal(rowSums(pm), rep(1, 1000))
  
  data("spe_test")
  
  spe <- readHoodData(spe, anno_col = "celltypes")
  
  fnc <- findNearCells(spe, k = 100)
  
  pm1 <- scanHoods(fnc$distance)
  
  pm2 <-scanHoods(fnc$distance, mode = "smoothFadeout")
  
  expect_gt(var(pm1[1,]), var(pm2[1,]))
  
  expect_error(scanHoods(fnc$distance, tau = "xyz"))
})
