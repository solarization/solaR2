test_that('declination in fSolD',{
    d <- fBTd(year = 2023)
    lat <- 37.2
    solD <- fSolD(d, lat)
    decl_expected <- read.csv('solD.csv')$decl
    expect_equal(solD$decl, decl_expected)
})

test_that('eccentricity in fSolD',{
    d <- fBTd(year = 2023)
    lat <- 37.2
    solD <- fSolD(d, lat)
    eo_expected <- read.csv('solD.csv')$eo
    expect_equal(solD$eo, eo_expected)
})

test_that('equation of time in fSolD',{
    d <- fBTd(year = 2023)
    lat <- 37.2
    solD <- fSolD(d, lat)
    eot_expected <- read.csv('solD.csv')$EoT
    expect_equal(solD$EoT, eot_expected)
})

test_that('solar time in fSolD',{
    d <- fBTd(year = 2023)
    lat <- 37.2
    solD <- fSolD(d, lat)
    ws_expected <- read.csv('solD.csv')$ws
    expect_equal(solD$ws, ws_expected)
})

test_that('extraterrestrial irradiance in fSolD',{
    d <- fBTd(year = 2023)
    lat <- 37.2
    solD <- fSolD(d, lat)
    bo0d_expected <- read.csv('solD.csv')$Bo0d
    expect_equal(solD$Bo0d, bo0d_expected)
})
