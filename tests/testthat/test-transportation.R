context('Transportation modules')
load('test-data/service_outputq.rda')
load('test-data/load_factorsq.rda')
load('test-data/final_energyq.rda')
load('test-data/refined_liquidsq.rda')
load('test-data/transp_norm.rda')

queries <- list(service_outputq, load_factorsq, final_energyq, refined_liquidsq)
queries <- stats::setNames(queries, c("Service output", "Load factors", "Final energy", "Refined liquids"))

test_that('GETQ Mode returns correct query titles', {
    expect_match(module.service_output(iamrpt:::GETQ), 'Service output')
    expect_match(module.load_factors(iamrpt:::GETQ), 'Load factors')
    expect_equal(module.final_energy(iamrpt:::GETQ), c("Final energy", "Refined liquids"))
    expect_equal(module.service_intensity(iamrpt:::GETQ), c("Final energy", "Refined liquids", "Service output"))
    expect_equal(module.pm_emissions(iamrpt:::GETQ), c("Service output", "Load factors"))
    })

test_that('Transportation models return transportation data', {
    aggkeys <- NA
    aggfn <- NA
    years <- '2000:2050'
    filters <- NA
    ounit <- NA
    expect_identical(module.service_output(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(transp_norm$`Service output`, year>=2000, year<=2050))
    expect_identical(module.load_factors(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(transp_norm$`Load factors`, year>=2000, year<=2050))
    expect_identical(module.final_energy(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(transp_norm$`Final energy`, year>=2000, year<=2050))
    expect_identical(module.service_intensity(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(transp_norm$`Service intensity`, year>=2000, year<=2050))
    expect_identical(module.pm_emissions(iamrpt:::RUN, queries, aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(transp_norm$`PM emissions`, year>=2000, year<=2050))

    years <- NA
    filters <- '(==; region;India), (==; service;Freight), (==;mode;Road), (==;submode;LHDT)'
    expect_identical(module.service_output(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(transp_norm$`Service output`, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.load_factors(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(transp_norm$`Load factors`, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.final_energy(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(transp_norm$`Final energy`, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.service_intensity(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(transp_norm$`Service intensity`, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.pm_emissions(iamrpt:::RUN, queries, aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(transp_norm$`PM emissions`, service=='Freight', mode=='Road', submode=='LHDT'))

    filters <- '(==; region;India), (==;service;Passenger), (==;mode;Road), (==;submode;4W), (==;fuel;Electricity)'
    expect_identical(module.final_energy(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(transp_norm$`Final energy`, service=='Passenger', mode=='Road', submode=='4W', fuel=='Electricity'))
})














