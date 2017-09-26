context('Electricity module')
load('test-data/electricityq.rda')


queries <- list(electricityq)
queries <- stats::setNames(queries, c("Electricity"))


test_that('GETQ Mode returns correct query title', {
    expect_match(module.electricity(iamrpt:::GETQ), 'Electricity')
})


test_that('Call to modules without aggkeys, aggfn, strtyr, endyr, filters, and ounit returns original data', {
    aggkeys <- NA
    aggfn <- NA
    strtyr <- NA
    endyr <- NA
    filters <- NA
    ounit <- NA
    expect_identical(module.electricity(iamrpt:::RUN, queries , aggkeys, aggfn, strtyr, endyr, filters, ounit),
                     queries$Electricity)
})
