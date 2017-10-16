context('Running queries')

test_that('Default queries are read.', {
    expect_true('parsed_queries' %in% names(private))
    ## Check on a query
    expect_true('GDP(MER)' %in% names(private$parsed_queries))
    gdpquery <- private$parsed_queries[['GDP(MER)']]
    expect_true(is.list(gdpquery))
    expect_identical(names(gdpquery), c('regions', 'query', 'title'))
    expect_identical(gdpquery$title, 'GDP(MER)')
    expect_identical(gdpquery$regions, character(0))
    expect_identical(gdpquery$query,
                     "<gdpQueryBuilder title=\"GDP(MER)\">\n  <axis1 name=\"region\">region</axis1>\n  <axis2 name=\"year\">gdp-mer</axis2>\n  <xPath buildList=\"true\" dataName=\"gdp-mer\" group=\"false\" sumAll=\"false\">GDP/gdp-mer/text()</xPath>\n  <comments/>\n</gdpQueryBuilder>")
})

test_that('We can parse a new query.', {
    xmlfilename <- 'test-data/test-query.xml'
    expect_false('Test Query' %in% names(private$parsed_queries))

    parseQueries(xmlfilename)
    expect_true('Test Query' %in% names(private$parsed_queries))
})


test_that('We can run a query.', {
    qr <- runQueries('Population', dirname(rgcam:::SAMPLE.GCAMDB),
                     basename(rgcam:::SAMPLE.GCAMDB))
    expect_identical(names(qr), 'Population')
    popdf <- qr[['Population']]
    expect_true(is.data.frame(popdf))
    expect_equal(nrow(popdf), 22)
    expect_identical(names(popdf), c("Units", "scenario", "region",   "year",
                                     "value", "rundate"))
})
