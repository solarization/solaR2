test_that('Dates in fSolD',{
    d <- fBTd(year = 2023)
    lat <- 37.2
    days_expected <- as.IDate(d)
    expect_equal(fSolD(d, lat)$Dates, days_expected)
})

test_that('declination in fSolD',{
    d <- fBTd(year = 2023)
    lat <- 37.2
    expect_equal(fSolD(d, lat)$decl, declination(d))
})

test_that('eccentricity in fSolD',{
    d <- fBTd(year = 2023)
    lat <- 37.2
    expect_equal(fSolD(d, lat)$eo, eccentricity(d))
})

test_that('equation of time in fSolD',{
    d <- fBTd()
    lat <- 37.2
    expect_equal(fSolD(d, lat)$EoT, eot(d))
})

test_that('solar time in fSolD',{
    d <- fBTd(year = 2023)
    lat <- 37.2
    expect_equal(fSolD(d, lat)$ws, sunrise(d, lat))
})

test_that('extraterrestrial irradiance in fSolD',{
    d <- fBTd(year = 2023)
    lat <- 37.2
    expect_equal(fSolD(d, lat)$Bo0d, bo0d(d, lat))
})
