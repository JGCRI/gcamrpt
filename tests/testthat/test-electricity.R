context('Electricity module')
load('test-data/electricityq.rda')


queries <- list(electricityq)
queries <- stats::setNames(queries, c("Electricity"))


test_that('GETQ Mode returns correct query title', {
    expect_match(module.electricity(iamrpt:::GETQ), 'Electricity')
})


test_that('Electricity module produces electricity data.', {
    aggkeys <- NA
    aggfn <- NA
    years <- '2000:2050'
    filters <- NA
    ounit <- NA
    expect_identical(module.electricity(iamrpt:::RUN, queries , aggkeys, aggfn,
                                        years, filters, ounit),
                     dplyr::filter(queries$Electricity, year>=2000, year<=2050))
})
