context('Transportation modules')
load('test-data/service_outputq.rda')
load('test-data/load_factorsq.rda')
load('test-data/final_energyq.rda')
load('test-data/refined_liquidsq.rda')
load('test-data/transp_normalized.rda')

service_norm <- transp_normalized[[1]]
load_norm <- transp_normalized[[2]]
energy_norm <- transp_normalized[[3]]
intensity_norm <- transp_normalized[[4]]
pm_norm <- transp_normalized[[5]]

queries <- list(service_outputq, load_factorsq, final_energyq, refined_liquidsq)
queries <- stats::setNames(queries,
                           c("Transportation Service Output", "Load Factors",
                             "Transportation Final Energy", "Transportation Refined Liquids"))

test_that('GETQ Mode returns correct query titles', {
    expect_match(module.transportation_service_output(GETQ), 'Transportation Service Output')
    expect_match(module.load_factors(GETQ), 'Load Factors')
    expect_equal(module.transportation_final_energy(GETQ),
                 c("Transportation Final Energy", "Transportation Refined Liquids"))
    expect_equal(module.transportation_service_intensity(GETQ),
                 c("Transportation Final Energy",
                   "Transportation Refined Liquids", "Transportation Service Output"))
    expect_equal(module.transportation_pm_emissions(GETQ), c("Transportation Service Output",
                                              "Load Factors"))
    })

test_that('Transportation models return transportation data', {
    aggkeys <- NA
    aggfn <- NA
    years <- '2000:2050'
    filters <- NA
    expect_identical(
        module.transportation_service_output(RUN, queries, aggkeys, aggfn, years,
                                                          '(==; service; Passenger)', 'million pass-km'),
        dplyr::filter(service_norm, year>=2000, year<=2050, service=='Passenger'))
    expect_identical(module.load_factors(RUN, queries , aggkeys, aggfn, years,
                                         filters, 'load/veh'),
                     dplyr::filter(load_norm, year>=2000, year<=2050))
    expect_identical(module.transportation_final_energy(RUN, queries, aggkeys, aggfn, years,
                                         filters, 'EJ'),
                     dplyr::filter(energy_norm, year>=2000, year<=2050))
    expect_identical(module.transportation_service_intensity(RUN, queries, aggkeys, aggfn,
                                              years, filters, 'TJ'),
                     dplyr::filter(intensity_norm, year>=2000, year<=2050))
    expect_identical(module.transportation_transportation_pm_emissions(RUN, queries, aggkeys, aggfn, years,
                                         filters, 'g'),
                     dplyr::filter(pm_norm, year>=2000, year<=2050))

    years <- NA
    filters <- '(==; region;India), (==; service;Freight), (==;mode;Road), (==;submode;LHDT)'
    expect_identical(module.transportation_service_output(RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(service_norm, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.load_factors(RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(load_norm, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.transportation_final_energy(RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(energy_norm, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.transportation_service_intensity(RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(intensity_norm, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.transportation_pm_emissions(RUN, queries, aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(pm_norm, service=='Freight', mode=='Road', submode=='LHDT'))

    filters <- '(==; region;India), (==;service;Passenger), (==;mode;Road), (==;submode;4W), (==;fuel;Electricity)'
    expect_identical(module.transportation_final_energy(RUN, queries , aggkeys,
                                                        aggfn, years, filters,
                                                        ounit),
                     dplyr::filter(energy_norm, service=='Passenger',
                                   mode=='Road', submode=='4W',
                                   fuel=='Electricity'))
    expect_identical(module.transportation_service_intensity(RUN, queries ,
                                                             aggkeys, aggfn,
                                                             years, filters,
                                                             ounit),
                     dplyr::filter(intensity_norm, service=='Passenger',
                                   mode=='Road', submode=='4W',
                                   fuel=='Electricity'))
})














