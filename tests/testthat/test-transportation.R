context('Transportation modules')
library(magrittr, warn.conflicts=FALSE)

load('test-data/service_outputq.rda')
load('test-data/load_factorsq.rda')
load('test-data/final_energyq.rda')
load('test-data/refined_liquidsq.rda')
load('test-data/trans_standardized.rda')

service_norm <- trans_standardized[[1]]
load_norm <- trans_standardized[[2]]
energy_norm <- trans_standardized[[3]]
intensity_norm <- trans_standardized[[4]]
pm_norm <- trans_standardized[[5]]

queries <- list(service_outputq, load_factorsq, final_energyq, refined_liquidsq)
queries <- stats::setNames(queries,
                           c("Transportation Service Output", "Transportation Load Factors",
                             "Transportation Final Energy", "Refined Liquids"))

test_that('GETQ Mode returns correct query titles', {
    expect_match(module.pass_trans_service_output(GETQ),
                 'Transportation Service Output')
    expect_match(module.frgt_trans_service_output(GETQ), 'Transportation Service Output')
    expect_equal(module.pass_trans_final_energy(GETQ),
                 c("Transportation Final Energy", "Refined Liquids"))
    expect_equal(module.frgt_trans_final_energy(GETQ),
                 c("Transportation Final Energy", "Refined Liquids"))
    expect_equal(module.pass_trans_service_intensity(GETQ),
                 c("Transportation Final Energy",
                   "Transportation Service Output",
                   "Refined Liquids"))
    expect_equal(module.frgt_trans_service_intensity(GETQ),
                 c("Transportation Final Energy",
                   "Transportation Service Output",
                   "Refined Liquids"))
    expect_match(module.pass_trans_load_factor(GETQ),
                 'Transportation Load Factors')
    expect_match(module.frgt_trans_load_factor(GETQ),
                 'Transportation Load Factors')

    expect_equal(module.transportation_pm_emissions(GETQ), c("Transportation Service Output",
                                                             "Transportation Load Factors"))
    })

test_that('Transportation models return transportation data', {
    aggkeys <- NA
    aggfn <- NA
    years <- '2000:2050'
    filters <- NA
    filter_operator <- NA
    expect_equal(
        module.pass_trans_service_output(RUN, queries, aggkeys, aggfn, years,
                                         filters, filter_operator, 'million pass-km'),
        dplyr::filter(service_norm, year>=2000, year<=2050, service=='Passenger'))
    expect_equal(
        module.frgt_trans_service_output(RUN, queries, aggkeys, aggfn, years,
                                         filters, filter_operator, 'million tonne-km'),
        dplyr::filter(service_norm, year>=2000, year<=2050, service=='Freight'))
    ## These modules should be able to change the count
    expect_equal(
        module.pass_trans_service_output(RUN, queries, aggkeys, aggfn, years,
                                         filters, filter_operator, 'thousand pass-km') %>%
         dplyr::mutate(value=signif(value,3)),
        dplyr::filter(service_norm, year>=2000, year<=2050,
                      service=='Passenger') %>%
          dplyr::mutate(value=signif(value*1000,3), Units='thousand pass-km'))
    expect_equal(
        module.frgt_trans_service_output(RUN, queries, aggkeys, aggfn, years,
                                         filters, filter_operator, 'billion tonne-km') %>%
         dplyr::mutate(value=signif(value,3)),
        dplyr::filter(service_norm, year>=2000, year<=2050,
                      service=='Freight') %>%
          dplyr::mutate(value=signif(value*0.001,3), Units='billion tonne-km'))


    expect_equal(module.pass_trans_load_factor(RUN, queries , aggkeys,
                                                   aggfn, years, filters, filter_operator,
                                                   'pass / veh'),
                     dplyr::filter(load_norm, year>=2000, year<=2050, service=='Passenger'))
    expect_equal(module.frgt_trans_load_factor(RUN, queries , aggkeys,
                                                   aggfn, years, filters, filter_operator,
                                                   'tonnes / veh'),
                     dplyr::filter(load_norm, year>=2000, year<=2050,
                                   service=='Freight'))
    ## Freight should be able to convert mass units.  Passenger doesn't do any
    ## unit conversion.
    expect_equal(module.frgt_trans_load_factor(RUN, queries , aggkeys,
                                                   aggfn, years, filters, filter_operator,
                                                   'kT / veh'),
                     dplyr::filter(load_norm, year>=2000, year<=2050,
                                   service=='Freight') %>%
                       dplyr::mutate(value=1e-3*value, Units='kT / veh'))


    expect_equal(module.pass_trans_final_energy(RUN, queries, aggkeys, aggfn, years,
                                                    filters, filter_operator, 'EJ'),
                     dplyr::filter(energy_norm, year>=2000, year<=2050,
                                   service=='Passenger'))
    expect_equal(module.frgt_trans_final_energy(RUN, queries, aggkeys, aggfn, years,
                                                    filters, filter_operator, 'EJ'),
                     dplyr::filter(energy_norm, year>=2000, year<=2050,
                                   service=='Freight'))
    ## Both of these should be able to convert energy units
    expect_equal(module.pass_trans_final_energy(RUN, queries, aggkeys, aggfn, years,
                                                    filters, filter_operator, 'TJ'),
                     dplyr::filter(energy_norm, year>=2000, year<=2050,
                                   service=='Passenger') %>%
                     dplyr::mutate(value=1e6*value, Units='TJ'))
    expect_equal(module.frgt_trans_final_energy(RUN, queries, aggkeys, aggfn, years,
                                                    filters, filter_operator, 'TJ'),
                     dplyr::filter(energy_norm, year>=2000, year<=2050,
                                   service=='Freight') %>%
                     dplyr::mutate(value=1e6*value, Units='TJ'))



    expect_equal(module.pass_trans_service_intensity(RUN, queries, aggkeys, aggfn,
                                                     years, filters, filter_operator, 'EJ / million pass-km'),
                 dplyr::filter(intensity_norm, year>=2000, year<=2050,
                               service=='Passenger'))
    expect_equal(module.frgt_trans_service_intensity(RUN, queries, aggkeys, aggfn,
                                                     years, filters, filter_operator,
                                                     'EJ / million tonne-km'),
                 dplyr::filter(intensity_norm, year>=2000, year<=2050,
                               service=='Freight'))
    ## should be able to convert energy counts, and for freight, mass
    expect_equal(module.pass_trans_service_intensity(RUN, queries, aggkeys, aggfn,
                                                         years, filters, filter_operator, 'TJ / thous pass-km'),
                     dplyr::filter(intensity_norm, year>=2000, year<=2050,
                                   service=='Passenger') %>%
                       dplyr::mutate(value=1e3*value,
                                     Units='TJ / thous pass-km'))
    expect_equal(module.frgt_trans_service_intensity(RUN, queries, aggkeys, aggfn,
                                                         years, filters, filter_operator,
                                                         'EJ / MT-km'),
                     dplyr::filter(intensity_norm, year>=2000, year<=2050,
                                   service=='Freight') %>%
                       dplyr::mutate(Units='EJ / MT-km')) # "million tonnes" and
                                        # "MT" are the same.


    expect_warning(module.sales(RUN, queries, aggkeys, aggfn, years, filters, filter_operator,
                                NA))
    ## expect_equal(module.transportation_pm_emissions(RUN, queries, aggkeys, aggfn, years,
    ##                                      filters, 'g'),
    ##                  dplyr::filter(pm_norm, year>=2000, year<=2050))
    expect_warning(module.transportation_pm_emissions(RUN, queries, aggkeys, aggfn, years,
                                                      filters, filter_operator, 'g'))

    years <- NA
    ounit <- NA
    filters <- '(==; region;India), (==; service;Freight), (==;mode;Road), (==;submode;LHDT)'
    filter_operator <- 'AND'
    expect_equal(module.frgt_trans_service_output(RUN, queries , aggkeys, aggfn, years, filters, filter_operator, ounit),
                     dplyr::filter(service_norm, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_equal(module.frgt_trans_load_factor(RUN, queries , aggkeys, aggfn, years, filters, filter_operator, ounit),
                     dplyr::filter(load_norm, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_equal(module.frgt_trans_final_energy(RUN, queries , aggkeys, aggfn, years, filters, filter_operator, ounit),
                     dplyr::filter(energy_norm, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_equal(module.frgt_trans_service_intensity(RUN, queries , aggkeys, aggfn, years, filters, filter_operator, ounit),
                     dplyr::filter(intensity_norm, service=='Freight', mode=='Road', submode=='LHDT'))
    ## expect_equal(module.transportation_pm_emissions(RUN, queries, aggkeys, aggfn, years, filters, ounit),
    ##                  dplyr::filter(pm_norm, service=='Freight', mode=='Road', submode=='LHDT'))

    filters <- '(==; region;India), (==;service;Passenger), (==;mode;Road), (==;submode;4W), (==;fuel;Electricity)'
    filter_operator <- 'AND'
    expect_equal(module.pass_trans_final_energy(RUN, queries , aggkeys,
                                                    aggfn, years, filters, filter_operator,
                                                    ounit),
                     dplyr::filter(energy_norm, service=='Passenger',
                                   mode=='Road', submode=='4W',
                                   fuel=='Electricity'))
    expect_equal(module.pass_trans_service_intensity(RUN, queries ,
                                                         aggkeys, aggfn,
                                                         years, filters, filter_operator,
                                                         ounit),
                     dplyr::filter(intensity_norm, service=='Passenger',
                                   mode=='Road', submode=='4W',
                                   fuel=='Electricity'))
})














