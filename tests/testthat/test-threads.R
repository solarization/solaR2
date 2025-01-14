test_that("data.table threads are set correctly", {
  expect_equal(data.table::getDTthreads(), 2)
})
