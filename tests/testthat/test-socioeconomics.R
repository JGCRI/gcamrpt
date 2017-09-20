context('Socioeconomics modules')
load('test-data/popq.rda')
load('test-data/gdp_merq.rda')
load('test-data/pcgdp_pppq.rda')

queries <- list(popq, gdp_merq, pcgdp_pppq)
queries <- stats::setNames(queries, c("Population", "GDP(MER)", "pcGDP(PPP)"))


test_that('GETQ Mode returns correct query titles', {
    expect_match(module.population(iamrpt:::GETQ), 'Population')
    expect_match(module.gdp_mer_(iamrpt:::GETQ), 'GDP(MER)')
    expect_match(module.pcgdp_ppp_(iamrpt:::GETQ), 'pcGDP(PPP)')

})

test_that('Call to modules without aggkeys, aggfn, strtyr, endyr, filters, and ounit returns original data', {
              aggkeys <- NA
              aggfn <- NA
              strtyr <- NA
              endyr <- NA
              filters <- NA
              ounit <- NA
              expect_identical(module.population(iamrpt:::RUN, sociec[['Population']] , aggkeys, aggfn, strtyr, endyr, filters, ounit),
                               popq)
              expect_identical(module.gdp_mer_(iamrpt:::RUN, sociec[['GDP(MER)']] , aggkeys, aggfn, strtyr, endyr, filters, ounit),
                               popq)
              expect_identical(module.pcgdp_ppp_(iamrpt:::RUN, sociec[['pcGDP(PPP)']] , aggkeys, aggfn, strtyr, endyr, filters, ounit),
                               popq)
})
