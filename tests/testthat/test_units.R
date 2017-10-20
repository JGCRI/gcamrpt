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


test_that('Count unit conversion works.', {
    expect_equal(unitconv_counts('Thous', 'mil'), 0.001)
    expect_equal(unitconv_counts('billion m^2', 'm^2 (thousands)'), 1e6)
    expect_equal(unitconv_counts('hundreds', ''), 100)
    expect_equal(unitconv_counts('trillion', 'billion', TRUE), 1e-3)
    expect_warning({u <- unitconv_counts('zillion', '')},
                   'not recognized as a counting unit')
    expect_true(is.na(u))
    expect_error(unitconv_counts(c('million', 'million'), 'billion'))
    expect_error(unitconv_counts(10, 20))
})

test_that('Dollar year conversion works.', {
    unitstr <- c('1980 USD', '1990 US$', '2010 USD')
    df <- data.frame(value=1, Units=unitstr, stringsAsFactors=FALSE)

    ## generates a warning due to multiple units, but should still work.
    expect_warning({cf <- unitconv_usdollar(df$Units, '90USD')},
                   'multiple units')

    expect_equal(signif(cf,3), c(1.50, 1.0, 0.66))

    expect_warning({cf <- unitconv_usdollar(df$Units, '90USD', TRUE)},
                   'multiple units')
    expect_equal(signif(cf,3), c(0.665, 1.0, 1.520))
})


test_that('Energy unit conversion works.', {

    expect_equal(unitconv_energy('EJ', 'TJ'), 1e6)
    expect_equal(unitconv_energy('EJ', 'MJ'), 1e12)
    expect_equal(unitconv_energy('EJ', 'MWh', TRUE), 3.6e-9)

    expect_equal(unitconv_energy('TJ', 'MJ'), 1e6)
    expect_equal(unitconv_energy('TJ', 'MWh', TRUE), 3.6e-3)

    expect_equal(unitconv_energy('MJ', 'MWh', TRUE), 3.6e3)

    for(from in rownames(energyconv)) {
        expect_equal(energyconv[from,from], 1.0)
        for(to in colnames(energyconv)) {
            ## expect_equal should take care of floating point rounding.
            expect_equal(energyconv[from][to], 1.0/energyconv[to][from])
        }
    }

})

test_that('Mass unit conversion works.', {
    expect_equal(unitconv_mass('kg', 'Mg'), 1.0e-3)
    expect_equal(unitconv_mass('kg', 'Gg'), 1.0e-6)
    expect_equal(unitconv_mass('kg', 'Tg'), 1.0e-9)
    expect_equal(unitconv_mass('kg', 'Pg'), 1.0e-12)
    expect_equal(unitconv_mass('kg', 'kT'), 1.0e-6)
    expect_equal(unitconv_mass('kg', 'MT'), 1.0e-9)
    expect_equal(unitconv_mass('kg', 'tonne'), 1.0e-3)

    expect_equal(unitconv_mass('Mg', 'Gg'), 1.0e-3)
    expect_equal(unitconv_mass('Mg', 'Tg'), 1.0e-6)
    expect_equal(unitconv_mass('Mg', 'Pg'), 1.0e-9)
    expect_equal(unitconv_mass('Mg', 'kT'), 1.0e-3)
    expect_equal(unitconv_mass('Mg', 'MT'), 1.0e-6)
    expect_equal(unitconv_mass('Mg', 'tonne'), 1)

    expect_equal(unitconv_mass('Gg', 'Tg'), 1.0e-3)
    expect_equal(unitconv_mass('Gg', 'Pg'), 1.0e-6)
    expect_equal(unitconv_mass('Gg', 'kT'), 1)
    expect_equal(unitconv_mass('Gg', 'MT'), 1e-3)
    expect_equal(unitconv_mass('Gg', 'tonne'), 1e3)

    expect_equal(unitconv_mass('Tg', 'Pg'), 1.0e-3)
    expect_equal(unitconv_mass('Tg', 'kT'), 1e3)
    expect_equal(unitconv_mass('Tg', 'MT'), 1)
    expect_equal(unitconv_mass('Tg', 'tonne'), 1e6)

    expect_equal(unitconv_mass('Pg', 'kT'), 1e6)
    expect_equal(unitconv_mass('Pg', 'MT'), 1e3)
    expect_equal(unitconv_mass('Pg', 'tonne'), 1e9)

    expect_equal(unitconv_mass('kT', 'MT'), 1e-3)
    expect_equal(unitconv_mass('kT', 'tonne'), 1e3)

    expect_equal(unitconv_mass('MT', 'tonne'), 1e6)

    for(from in rownames(massconv)) {
        expect_equal(massconv[from,from], 1.0)
        for(to in colnames(massconv)) {
            ## expect_equal should take care of floating point rounding.
            expect_equal(massconv[from][to], 1.0/massconv[to][from])
        }
    }
})


test_that('Energy unit conversion warns on bad unit strings.', {
    expect_warning({cf <- unitconv_energy('kWh', 'MWh')},
                   'kWh is not recognized as a unit for energy')
    expect_true(is.na(cf))

    expect_warning({cf <- unitconv_energy('TJEJ', 'MWh')},
                   'J matches multiple units')
    expect_true(is.na(cf))
})
