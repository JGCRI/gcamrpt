context('Unit conversion functions')


test_that('Parsing years works as designed.', {
    testyears <- c('47USD', '50US$', '16 USD',
                   '104', 'Thousand1922USD',
                   'Trillion 2112$')
    years <- yrparse(testyears)
    expect_equal(years,
                 c(1947, 1950, 2016, 2004, 1922, 2112))

    testyears <- c('the distant future', '2000')
    expect_warning({years <- yrparse(testyears)},
                   'The following strings did not contain')
    expect_equal(years, c(NA, 2000))
})


test_that('Clamp utility works with scalar boundaries.', {
    y <- c(1922, 1947, 1980, 2010, 2020)
    yc <- clamp(y, 1947, 2010)
    expect_equal(yc, c(1947, 1947, 1980, 2010, 2010))
})

test_that('Clamp utility works with vector boundaries.', {
    y <- c(1922, 1947, 1980, 2010, 2020)
    lo <- c(1900, 1950, 1947, 1947, 1947)
    hi <- c(2010, 2010, 2010, 2000, 2050)
    yc <- clamp(y, lo, hi)
    expect_equal(yc, c(1922, 1950, 1980, 2000, 2020))
})


test_that('Dollar year conversion works.', {
    unitstr <- c('1980 USD', '1990 US$', '2010 USD')
    df <- data.frame(value=1, Units=unitstr)

    ## generates a warning due to multiple units, but should still work.
    expect_warning({convdf <- unitconv_usdollar(df, '90USD')},
                   'multiple units')

    expect_equal(signif(convdf$value,3), c(1.50, 1.0, 0.66))
})


test_that('Energy unit conversion works.',
      {
    df <- tibble::tibble(value=1, Units='EJ')
    convdf <- unitconv_energy(df, NA)
    expect_identical(df, convdf)

    convdf <- unitconv_energy(df, 'EJ')
    expect_equal(df, convdf)

    convdf <- unitconv_energy(df, 'MWh')
    convdf$value <- signif(convdf$value, 3)
    dfq <- tibble::tibble(value=2.78e8, Units='MWh')
    expect_equal(dfq, convdf)

    df$Units <- 'MWh'
    convdf <- unitconv_energy(df, 'EJ')
    convdf$value <- signif(convdf$value, 3)
    dfq <- tibble::tibble(value=3.6e-9, Units='EJ')
    expect_equal(dfq, convdf)

})
