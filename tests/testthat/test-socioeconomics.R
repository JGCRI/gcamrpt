context('Socioeconomics modules')

load('test-data/popq.rda')
load('test-data/gdp_merq.rda')
load('test-data/pcgdp_pppq.rda')

queries <- list(popq, gdp_merq, pcgdp_pppq)
queries <- stats::setNames(queries, c("Population", "GDP(MER)", "pcGDP(PPP)"))


test_that('GETQ Mode returns correct query titles', {
    expect_match(module.population(iamrpt:::GETQ), 'Population')
    expect_match(module.pcgdp_ppp_(iamrpt:::GETQ), 'pcGDP\\(PPP\\)')
    expect_match(module.gdp_mer_(iamrpt:::GETQ), "GDP\\(MER\\)")

})

test_that('Socioeconomics models produce data', {
              aggkeys <- NA
              aggfn <- NA
              years <- '2000:2050'
              filters <- NA

              ## All of these should be passing in the original unit, just to
              ## make sure that unit conversion runs
              expect_identical(module.population(iamrpt:::RUN, queries, aggkeys,
                                                 aggfn, years, filters,
                                                 'thous'),
                               dplyr::filter(queries$Population, year>=2000, year<=2050))
              expect_identical(module.gdp_mer_(iamrpt:::RUN, queries , aggkeys,
                                               aggfn, years, filters, 'Million1990US$'),
                               dplyr::filter(queries$`GDP(MER)`, year>=2000, year<=2050))
              expect_identical(module.pcgdp_ppp_(iamrpt:::RUN, queries ,
                                                 aggkeys, aggfn, years, filters,
                                                 'Thous80US$/per'),
                               dplyr::filter(queries$`pcGDP(PPP)`, year>=2000, year<=2050))


              ## Test a unit conversion on population.
              ## TODO: we should test at least one unit converison on each
              ## module.
              popmil <- module.population(iamrpt:::RUN, queries , aggkeys,
                                          aggfn, years, filters, 'millions')
              popqmil <- dplyr::filter(queries$Population, year>=2000,
                                       year<=2050)
              popqmil <- dplyr::mutate(popqmil, value = 1e-3*value)
              expect_identical(dplyr::select(popmil, -Units), dplyr::select(popqmil,-Units))
})


