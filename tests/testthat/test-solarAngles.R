test_that("declination in certain days", {
    expect_equal(signif(declination("2024-04-06"), 6),
                 0.117746)
})
