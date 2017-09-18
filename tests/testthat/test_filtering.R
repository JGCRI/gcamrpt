context('Filtering output variables')

load('test-data/popq.rda')     # loads the popq data frame

test_that('Filtering with no filters is a no-op', {
    popf <- filter(popq, NA, NA, NA)
    expect_identical(popq, popf)

    popf <- filter(popq, '', '', '')
    expect_identical(popq, popf)
})

test_that('Start and end years are applied correctly', {
    popf <- filter(popq, 2000, NA, NA)
    expect_identical(popf, dplyr::filter(popq, year >= 2000))

    popf <- filter(popq, NA, 2050, NA)
    expect_identical(popf, dplyr::filter(popq, year <= 2050))

    popf <- filter(popq, 2000, 2050, NA)
    expect_identical(popf, dplyr::filter(popq, year >= 2000, year <= 2050))

})

test_that('The == filter works', {
    popf <- filter(popq, NA, NA, '(==; region; USA)')
    expect_identical(popf, dplyr::filter(popq, region == 'USA'))
})

test_that('The != filter works', {
    popf <- filter(popq, NA, NA, '(!=; region; USA)')
    expect_identical(popf, dplyr::filter(popq, region != 'USA'))
})

test_that('Numeric filters work', {
    ## NB: should really test these against a table with floating point values.
    for(op in c('<', '>', '<=', '>=')) {
        testval <- 117670     # close to median but actually exists in the table
        filterstr <- paste0('( ', op, '; value; ', testval, ' )')
        popf <- filter(popq, NA, NA, filterstr)
        dpfilt <- lazyeval::interp(~ oper(value, testval), oper = as.name(op))
        expect_identical(popf, dplyr::filter_(popq, dpfilt),
                         info=paste('filter is not equivalent to dplyr::filter for op= ',
                         op))
    }
})

test_that('Regex filters work', {
    popqf <- dplyr::filter(popq, grepl('Africa', region))
    popf <- filter(popq, NA, NA, '(matches; region; Africa)')
    expect_identical(popf, popqf)

    popf <- filter(popq, NA, NA, '(matches; region; africa)')
    expect_equal(nrow(popf), 0)         # No regions with lower case "africa"

    popf <- filter(popq, NA, NA, '(matchesi; region; africa)')
    expect_identical(popf, popqf)       # case-insensitive match should work

    popqf <- dplyr::filter(popq, !grepl('Africa', region))
    popf <- filter(popq, NA, NA, '(notmatches; region; Africa)')
    expect_identical(popf, popqf)

    popf <- filter(popq, NA, NA, '(notmatches; region; africa)')
    expect_identical(popf, popq)        # all regions fail to match lower case

    popf <- filter(popq, NA, NA, '(notmatchesi; region; africa)')
    expect_identical(popf, popqf)
})

test_that('Multiple filters work', {
    popf <- filter(popq, NA, NA, '(matches; region; Africa), (<=; value; 227757), (>=; value; 152801)')
    expect_identical(popf, dplyr::filter(popq, grepl('Africa', region), value <= 227757, value >= 152801))
})

test_that('Filter options work when all used together', {
    popf <- filter(popq, 2000, 2050, '(matches; region; Africa), (<=; value; 227757), (>=; value; 152801)')
    expect_identical(popf,
                     dplyr::filter(popq,
                                   grepl('Africa', region),
                                   year >= 2000,
                                   year <= 2050,
                                   value <= 227757,
                                   value >= 152801))
})

test_that('Malformed filter string gives a warning and gets skipped', {
    expect_warning({popf <- filter(popq, 2000, 2050, 'matches region Africa')},
                   'matches region Africa')
    expect_identical(popf, dplyr::filter(popq, year >= 2000, year <= 2050))
})


test_that("One malformed filter doesn't affect good filters.", {
    expect_warning({popf <- filter(popq, NA, NA, '(matches; region; Africa, (<; value; 227757)')},
                   '\\(matches; region; Africa')
    expect_identical(popf, dplyr::filter(popq, value < 227757))
})
