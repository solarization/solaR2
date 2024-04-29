test_that("declination in certain days", {
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2024", 1:12, promDays, sep = "-")
    decl_expected <- c(-0.36271754, -0.22850166, -0.03191616, 0.17531794, 0.33246485, 0.40257826, 0.36439367, 0.22407398, 0.02730595, -0.17900474, -0.33862399, -0.40478283)
    expect_equal(declination(days), decl_expected)
})

test_that("eccentricity in certain days", {
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2024", 1:12, promDays, sep = "-")
    eccen_expected <- c(1.031597011, 1.023584222, 1.009655811, 0.9922617841, 0.9774306591, 0.969234456, 0.9683222212, 0.9774306591, 0.9928151334, 1.010197561, 1.0243677, 1.03142846)
    expect_equal(eccentricity(days), eccen_expected)
})

test_that("equation of time in certain days",{
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2024", 1:12, promDays, sep = "-")
    eot_expected <- c(-10.43576706, -14.09002583, -8.738887764, 0.1729562815, 3.315238963, 0.02486676242, -5.993574547, -2.815736959, 7.484636357, 15.69229884, 13.45701554, 4.08646007)
    expect_equal(eot(days), eot_expected)
})

#test_that("sunrise angle in certain days",{
#    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
#    days <- paste("2024", 1:12, promDays, sep = "-")
#    lat <- 40
#    sunrise_expected <- c()
#})
