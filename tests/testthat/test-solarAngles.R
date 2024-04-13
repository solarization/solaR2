test_that("declination in certain days", {
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2024", 1:12, promDays, sep = "-")
    decl_expected <- c(-0.36271754, -0.22850166, -0.03191616, 0.17531794, 0.33246485, 0.40257826, 0.36439367, 0.22407398, 0.02730595, -0.17900474, -0.33862399, -0.40478283)
    expect_equal(declination(days), decl_expected)
})

test_that("eccentricity in certain days", {
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2024", 1:12, promDays, sep = "-")
    eccen_expected <- c(-20.91696257,-13.61976641,-2.818878653, 9.414893347, 18.79191752, 23.01163673, 21.00743137, 12.78588828, 1.008871364, -11.04869045, -19.82109039,-23.2416248)
    expect_equal(eccentricity(days), eccen_expected)
})
