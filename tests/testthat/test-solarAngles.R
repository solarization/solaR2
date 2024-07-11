test_that("declination in certain days", {
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    decl_expected <- read.csv('solD.csv')$decl
    expect_equal(declination(days), decl_expected)
})

test_that("eccentricity in certain days", {
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    eccen_expected <- read.csv('solD.csv')$eo
    expect_equal(eccentricity(days), eccen_expected)
})

test_that("equation of time in certain days",{
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    eot_expected <- read.csv('solD.csv')$EoT
    eot_expected <- r2h(eot_expected*60)
    expect_equal(eot(days), eot_expected)
})

test_that("sunrise angle in certain days",{
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    lat <- 37.2
    sun_expected <- read.csv('solD.csv')$ws
    expect_equal(sunrise(days, lat), sun_expected)
})

test_that("extraterrestrial irradiation in certain days",{
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    lat <- 37.2
    bo0d_expected <- read.csv('solD.csv')$Bo0d
    expect_equal(bo0d(days, lat), bo0d_expected)
})

test_that("sun hour angle throughout the day",{
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    w_expected <- read.csv('solI.csv')$w
    expect_equal(sunHour(days), w_expected, tolerance = 3e-5)
})

test_that("zenith angle throughout the day",{
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    lat <- 37.2
    ThzS_expected <- read.csv('solI.csv')$cosThzS
    expect_equal(zenith(days, lat), ThzS_expected, tolerance = 1e-4)
})

test_that("azimuth angle throughout the day",{
    promDays <- c(17, 14, 15, 15, 15, 10, 18, 18, 18, 19, 18, 13)
    days <- paste("2023", 1:12, promDays, sep = "-")
    lat <- 37.2
    AzS_expected <- read.csv('solI.csv')$AzS
    expect_equal(azimuth(days, lat), AzS_expected, tolerance = 1e-4)
})
