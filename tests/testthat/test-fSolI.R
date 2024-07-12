test_that('Sun hour angle in fSolI',{
    d <- fBTd(year = 2023)
    BTi <- fBTi(d, '1 hour')
    lat <- 37.2
    solD <- fSolD(d, lat)
    solI <- fSolI(solD, BTi = BTi)
    w_expected <- read.csv('solI.csv')$w
    expect_equal(solI$w, w_expected, tolerance = 1e-4) 
})

test_that('Zenith angle in fSolI',{
    d <- fBTd(year = 2023)
    BTi <- fBTi(d, '1 hour')
    lat <- 37.2
    solD <- fSolD(d, lat)
    solI <- fSolI(solD, BTi = BTi)
    cosThzS_expected <- read.csv('solI.csv')$cosThzS
    expect_equal(solI$cosThzS, cosThzS_expected, tolerance = 1e-4)
})

test_that('Azimuth angle in fSolI',{
    d <- fBTd(year = 2023)
    BTi <- fBTi(d, '1 hour')
    lat <- 37.2
    solD <- fSolD(d, lat)
    solI <- fSolI(solD, BTi = BTi)
    AzS_expected <- read.csv('solI.csv')$AzS
    expect_equal(solI$AzS, AzS_expected, tolerance = 1e-4)
})

test_that('Solar altitude angle in fSolI',{
    d <- fBTd(year = 2023)
    BTi <- fBTi(d, '1 hour')
    lat <- 37.2
    solD <- fSolD(d, lat)
    solI <- fSolI(solD, BTi = BTi)
    AlS_expected <- read.csv('solI.csv')$AlS
    expect_equal(solI$AlS, AlS_expected, tolerance = 1e-4)
})

test_that('Extraterrestrial irradiance in fSolI',{
    d <- fBTd(year = 2023)
    BTi <- fBTi(d, '1 hour')
    lat <- 37.2
    solD <- fSolD(d, lat)
    solI <- fSolI(solD, BTi = BTi)
    Bo0_expected <- read.csv('solI.csv')$Bo0
    expect_equal(solI$Bo0, Bo0_expected, tolerance = 1e-4)
})
