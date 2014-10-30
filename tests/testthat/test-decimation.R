test_that("decimation", {
  full <- c(0, 0, 0, 0, NA, NA, 0, 0)
  short <- downsample.vector(full, 2)
  
  expect_equal(short, c(0,0,NA,0))
})