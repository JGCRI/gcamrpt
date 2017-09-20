context('Population module')
load('test-data/popq.rda')


test_that('GETQ Mode returns Population query title', {
    expect_match(module.population(iamrpt:::GETQ), 'Population')
})

test_that('Call to Population module without aggkeys, aggfn, strtyr, endyr, filters, and ounit
          returns original data', {
    aggkeys <- NA
    aggfn <- NA
    strtyr <- NA
    endyr <- NA
    filters <- NA
    ounit <- NA
    expect_identical(module.population(iamrpt:::RUN, popq, aggkeys, aggfn, strtyr, endyr, filters, ounit),
                     popq)
})
