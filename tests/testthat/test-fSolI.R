test_that('Dates in fSolI',{
    d <- fBTd(year = 2023)
    lat <- 37.2
    solD <- fSolD(d, lat)
    days_expected <- fBTi(fBTd(year = 2023),
                          '1 hour')
    expect_equal(fSolI(solD)$Dates, days_expected)
})

test_that('Solar time in fSolI',{
    d <- fBTd(year = 2023)
    BTi <- fBTi(d, '1 hour')
    lat <- 37.2
    solD <- fSolD(d, lat)
    expect_equal(fSolI(solD, BTi = BTi)$w, sunHour(d, BTi))
})

test_that('Zenith angle in fSolI',{
    d <- fBTd(year = 2023)
    BTi <- fBTi(d, '1 hour')
    lat <- 37.2
    solD <- fSolD(d, lat)
    expect_equal(fSolI(solD, BTi = BTi)$cosThzS, zenith(d, lat, BTi))
})

test_that('Azimuth angle in fSolI',{
    d <- fBTd(year = 2023)
    BTi <- fBTi(d, '1 hour')
    lat <- 37.2
    solD <- fSolD(d, lat)
    expect_equal(fSolI(solD, BTi = BTi)$cosAzS, azimuth(d, lat, BTi))
})
