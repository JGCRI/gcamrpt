context('Emissions module')
load('test-data/BCbysubsectorq.rda')
load('test-data/CH4bysubsectorq.rda')
load('test-data/CObysubsectorq.rda')
load('test-data/CO2bysubsectorq.rda')
load('test-data/GHGbyregionq.rda')
load('test-data/GHGbytechq.rda')
load('test-data/NH3bysubsectorq.rda')
load('test-data/NMVOCbysubsectorq.rda')
load('test-data/NOxbysubsectorq.rda')
load('test-data/OCbysubsectorq.rda')
load('test-data/SO2bysubsectorq.rda')

queries <- list(BCbysubsectorq,
                CH4bysubsectorq,
                CObysubsectorq,
                CO2bysubsectorq,
                GHGbyregionq,
                GHGbytechq,
                NH3bysubsectorq,
                NMVOCbysubsectorq,
                NOxbysubsectorq,
                OCbysubsectorq,
                SO2bysubsectorq)
queries <- stats::setNames(queries, c("BC emissions by subsector",
                                      "CH4 emissions by subsector",
                                      "CO emissions by subsector",
                                      "CO2 emissions by subsector",
                                      "GHG emissions by region",
                                      "GHG emissions by technology",
                                      "NH3 emissions by subsector",
                                      "NMVOC emissions by subsector",
                                      "NOx emissions by subsector",
                                      "OC emissions by subsector",
                                      "SO2 emissions by subsector"))

test_that('GETQ Mode returns correct query titles', {
    expect_match(module.bc_emissions_by_subsector(iamrpt:::GETQ), 'BC emissions by subsector')
    expect_match(module.ch4_emissions_by_subsector(iamrpt:::GETQ), 'CH4 emissions by subsector')
    expect_match(module.co_emissions_by_subsector(iamrpt:::GETQ), 'CO emissions by subsector')
    expect_match(module.co2_emissions_by_subsector(iamrpt:::GETQ), 'CO2 emissions by subsector')
    expect_match(module.ghg_emissions_by_region(iamrpt:::GETQ), 'GHG emissions by region')
    expect_match(module.ghg_emissions_by_technology(iamrpt:::GETQ), 'GHG emissions by technology')
    expect_match(module.nh3_emissions_by_subsector(iamrpt:::GETQ), 'NH3 emissions by subsector')
    expect_match(module.nmvoc_emissions_by_subsector(iamrpt:::GETQ), 'NMVOC emissions by subsector')
    expect_match(module.nox_emissions_by_subsector(iamrpt:::GETQ), 'NOx emissions by subsector')
    expect_match(module.oc_emissions_by_subsector(iamrpt:::GETQ), 'OC emissions by subsector')
    expect_match(module.so2_emissions_by_subsector(iamrpt:::GETQ), 'SO2 emissions by subsector')
})

test_that('Emissions modules produces emissions data.', {
    aggkeys <- NA
    aggfn <- NA
    years <- '2000:2050'
    filters <- NA
    filter_operator <- NA
    ounit <- NA
    expect_identical(module.bc_emissions_by_subsector(iamrpt:::RUN, queries , aggkeys, aggfn,
                                        years, filters, filter_operator, ounit),
                     dplyr::filter(queries$`BC emissions by subsector`, year>=2000, year<=2050))
    expect_identical(module.ch4_emissions_by_subsector(iamrpt:::RUN, queries , aggkeys, aggfn,
                                                      years, filters, filter_operator, ounit),
                     dplyr::filter(queries$`CH4 emissions by subsector`, year>=2000, year<=2050))
    expect_identical(module.co_emissions_by_subsector(iamrpt:::RUN, queries , aggkeys, aggfn,
                                                      years, filters, filter_operator, ounit),
                     dplyr::filter(queries$`CO emissions by subsector`, year>=2000, year<=2050))
    expect_identical(module.co2_emissions_by_subsector(iamrpt:::RUN, queries , aggkeys, aggfn,
                                                      years, filters, filter_operator, ounit),
                     dplyr::filter(queries$`CO2 emissions by subsector`, year>=2000, year<=2050))
    expect_identical(module.ghg_emissions_by_region(iamrpt:::RUN, queries , aggkeys, aggfn,
                                                      years, filters, filter_operator, ounit),
                     dplyr::filter(queries$`GHG emissions by region`, year>=2000, year<=2050))
    expect_identical(module.ghg_emissions_by_technology(iamrpt:::RUN, queries , aggkeys, aggfn,
                                                      years, filters, filter_operator, ounit),
                     dplyr::filter(queries$`GHG emissions by technology`, year>=2000, year<=2050))
    expect_identical(module.nh3_emissions_by_subsector(iamrpt:::RUN, queries , aggkeys, aggfn,
                                                      years, filters, filter_operator, ounit),
                     dplyr::filter(queries$`NH3 emissions by subsector`, year>=2000, year<=2050))
    expect_identical(module.nmvoc_emissions_by_subsector(iamrpt:::RUN, queries , aggkeys, aggfn,
                                                      years, filters, filter_operator, ounit),
                     dplyr::filter(queries$`NMVOC emissions by subsector`, year>=2000, year<=2050))
    expect_identical(module.nox_emissions_by_subsector(iamrpt:::RUN, queries , aggkeys, aggfn,
                                                      years, filters, filter_operator, ounit),
                     dplyr::filter(queries$`NOx emissions by subsector`, year>=2000, year<=2050))
    expect_identical(module.oc_emissions_by_subsector(iamrpt:::RUN, queries , aggkeys, aggfn,
                                                      years, filters, filter_operator, ounit),
                     dplyr::filter(queries$`OC emissions by subsector`, year>=2000, year<=2050))
    expect_identical(module.so2_emissions_by_subsector(iamrpt:::RUN, queries , aggkeys, aggfn,
                                                      years, filters, filter_operator, ounit),
                     dplyr::filter(queries$`SO2 emissions by subsector`, year>=2000, year<=2050))
})
