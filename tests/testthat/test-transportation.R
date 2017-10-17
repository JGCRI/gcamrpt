context('Transportation modules')
load('test-data/service_outputq.rda')
load('test-data/load_factorsq.rda')
load('test-data/final_energyq.rda')
load('test-data/refined_liquidsq.rda')

queries <- list(service_outputq, load_factorsq, final_energyq)
queries <- stats::setNames(queries, c("Service output", "Load factors", "Final energy"))

test_that('GETQ Mode returns correct query titles', {
    expect_match(module.service_output(iamrpt:::GETQ), 'Service output')
    expect_match(module.load_factors(iamrpt:::GETQ), 'Load factors')
    expect_equal(module.final_energy(iamrpt:::GETQ), c("Final energy", "Refined liquids"))
    expect_equal(module.service_intensity(iamrpt:::GETQ), c("Final energy", "Refined liquids", "Service output"))
})

test_that('Transportation models return correct service/mode/submode factors', {
    serviceopts <- c("Freight", "Passenger")
    expect_equal(levels(factor(queries$`Service output`$service)),
                     serviceopts)
    expect_equal(levels(factor(queries$`Load factors`$service)),
                     serviceopts)
    expect_equal(levels(factor(queries$`Final energy`$service)),
                     serviceopts)
    expect_equal(levels(factor(queries$`Final energy`$service)),
                     serviceopts)

    modeopts <- c("Aviation", "Rail", "Road", "Shipping")
    expect_equal(levels(factor(queries$`Service output`$mode)),
                     modeopts)
    expect_equal(levels(factor(queries$`Load factors`$mode)),
                     modeopts)
    expect_equal(levels(factor(queries$`Final energy`$mode)),
                     modeopts)
    expect_equal(levels(factor(queries$`Final energy`$mode)),
                     modeopts)

    submodeopts <- c("2W", "3W", "4W", "Bus", "Domestic", "Freight Rail", "HHDT", "International", "LHDT", "MHDT", "Passenger Rail")
    expect_equal(levels(factor(queries$`Service output`$submode)),
                     submodeopts)
    expect_equal(levels(factor(queries$`Load factors`$submode)),
                     submodeopts)
    expect_equal(levels(factor(queries$`Final energy`$submode)),
                     submodeopts)
    expect_equal(levels(factor(queries$`Final energy`$submode)),
                     submodeopts)
})

test_that('Transportation models return transportation data', {
    aggkeys <- NA
    aggfn <- NA
    years <- '2000:2050'
    filters <- NA
    ounit <- NA
    expect_identical(module.service_output(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service output`, year>=2000, year<=2050))
    expect_identical(module.load_factors(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Load factors`, year>=2000, year<=2050))
    expect_identical(module.final_energy(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Final energy`, year>=2000, year<=2050))
    expect_identical(module.service_intensity(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service intensity`, year>=2000, year<=2050))

    years <- NA
    filters <- '(==; region;India), (==; service;Freight), (==;mode;Road), (==;submode;LHDT)'
    expect_identical(module.service_output(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service output`, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.load_factors(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Load factors`, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.final_energy(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Final energy`, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.service_intensity(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service intensity`, service=='Freight', mode=='Road', submode=='LHDT'))

    filters <- '(==; region;India), (==;service;Passenger), (==;mode;Road), (==;submode;4W), (==;fuel;Electricity)'
    expect_identical(module.service_output(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service output`, service=='Passenger', mode=='Road', submode=='4W', fuel=='Electricity'))
    expect_identical(module.load_factors(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Load factors`, service=='Passenger', mode=='Road', submode=='4W', fuel=='Electricity'))
    expect_identical(module.final_energy(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Final energy`, service=='Passenger', mode=='Road', submode=='4W', fuel=='Electricity'))
    expect_identical(module.service_intensity(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service intensity`, service=='Passenger', mode=='Road', submode=='4W', fuel=='Electricity'))
})














