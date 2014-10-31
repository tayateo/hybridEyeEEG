test_that("decimation_test1", {
  
  full <- c(0, 0, 0, 0, NA, NA, 0, 0)
  short <- decimate.with.na(full, 2)
  
  expect_equal(short, c(0,0,NA,0))
})

test_that("decimation_test2", {
  
  full <- c(0, 0, 0, NA, NA, NA, 0, 0)
  short <- decimate.with.na(full, 2)
  
  expect_equal(short, c(0,0,NA,0))
})


test_that("decimation_test3", {
  
  full <- c(0, 0, 0, NA, NA, NA, 0, 0, NA, 0, NA)
  short <- decimate.with.na(full, 2)
  
  expect_equal(short, c(0,0,NA,0, NA, NA))
})


test_that("decimation_test4", {
  
  full <- c(0, 0, 0, NA, NA, NA, 0, 0, NA, 0, NA, NA, NA)
  short <- decimate.with.na(full, 2)

  expect_equal(short, c(0,0,NA,0, NA, NA, NA))
})

test_that("decimation_test5", {
  
  full <- c(0, 0, 0, NA, NA, NA, 0, 0, NA, 0, NA, NA, NA, NA, NA)
  short <- decimate.with.na(full, 2)
  
  expect_equal(short, c(0,0,NA,0, NA, NA, NA, NA))
})


test_that("decimation_test6", {
  
  full <- c(NA, NA, NA, 0, 0, 0, NA, NA, NA, 0, 0, NA, 0, NA, NA, NA)
  short <- decimate.with.na(full, 2)
  
  expect_equal(short, c(NA, NA,  0, NA, NA,  0,  0, NA))
})

test_that("decimation_test7", {
  
  full <- c(NA, NA, NA, NA, 0, 0, NA, 0, NA, NA, NA, 0, 0, NA, 0, NA, NA, NA, NA, 0, 0, 0)
  short <- decimate.with.na(full, 2)
  
  expect_equal(short, c(NA, NA,  0, NA, NA, NA,  0,  0, NA, NA,  0))
})