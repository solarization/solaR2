test_that("declination in certain days", {
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    decl_expected <- c(-0.36271754, -0.22850166, -0.03191616, 0.17531794, 0.33246485, 0.40257826, 0.36439367, 0.22407398, 0.02730595, -0.17900474, -0.33862399, -0.40478283)
    expect_equal(declination(days), decl_expected, tolerance = 5e-2)
})

test_that("eccentricity in certain days", {
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    eccen_expected <- c(1.031597011, 1.023584222, 1.009655811, 0.9922617841, 0.9774306591, 0.969234456, 0.9683222212, 0.9774306591, 0.9928151334, 1.010197561, 1.0243677, 1.03142846)
    expect_equal(eccentricity(days), eccen_expected, tolerance = 5e-2)
})

test_that("equation of time in certain days",{
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    eot_expected <- c(-10.43576706, -14.09002583, -8.738887764, 0.1729562815, 3.315238963, 0.02486676242, -5.993574547, -2.815736959, 7.484636357, 15.69229884, 13.45701554, 4.08646007)
    expect_equal(eot(days), eot_expected)
})

test_that("sunrise angle in certain days",{
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    lat <- 37.2
    sun_expected <- c(-1.279346, -1.394509, -1.542589, -1.701939, -1.833257, -1.899013, -1.866667, -1.748141, -1.595423, -1.436782, -1.302937, -1.240372)
    expect_equal(sunrise(days, lat), sun_expected, tolerance = 5e-2)
})

test_that("extraterrestrial irradiation in certain days",{
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    lat <- 37.2
    bo0d_expected <- c(4747.900, 6152.753, 8037.885, 9884.093, 11095.193, 11569.031, 11274.255, 10216.467,  8553.634, 6600.204, 4982.855, 4292.563)
    expect_equal(bo0d(days, lat), bo0d_expected, tolerance = 5e-2)
})
