context('Output functions')

## Load some sample data to work with
load('test-data/popq.rda')
load('test-data/flrspcq.rda')

## Create a sample outputs structure
ref <- list(population=popq, floorspace=flrspcq)
expt1 <- list(population=dplyr::mutate(popq, scenario='Experiment 1'),
              floorspace=dplyr::mutate(flrspcq, scenario='Experiment 1'))
expt2 <- list(population=dplyr::mutate(popq, scenario='Experiment 2'),
              floorspace=dplyr::mutate(flrspcq, scenario='Experiment 2'))
rslt <- list(Reference=ref, 'Experiment 1'=expt1, 'Experiment 2'=expt2)
rsltmrg <- merge_scenarios(rslt)


test_that('merge_scenarios produces the correct format', {
    ## all of these tests run on the rsltmrg object that we created above.
    expect_true(is.list(rsltmrg))
    expect_equal(length(rsltmrg), 2)
    expect_identical(names(rsltmrg), c('population', 'floorspace'))
    expect_true(all(sapply(rsltmrg, is.data.frame)))

    expect_identical(names(rsltmrg$population),
                     c('Units', 'scenario', 'region', 'year', 'value'))
    expect_identical(unique(sort(rsltmrg$population$scenario)),
                     c('Experiment 1', 'Experiment 2', 'Reference'))
    expect_identical(dplyr::filter(rsltmrg$population, scenario == 'Reference'),
                     popq)

    expect_identical(names(rsltmrg$floorspace),
                     c('Units', 'scenario', 'region',
                       'building', 'nodeinput', 'building-node-input',
                       'year', 'value'))
    expect_identical(unique(sort(rsltmrg$floorspace$scenario)),
                     c('Experiment 1', 'Experiment 2', 'Reference'))
    expect_identical(dplyr::filter(rsltmrg$floorspace, scenario == 'Reference'),
                     flrspcq)
})

test_that('alternate_filename does the right thing with xyz.csv.', {
    dir <- tempdir()
    filename <- file.path(dir, 'xyz.csv')
    if(file.exists(filename))
        unlink(filename)
    on.exit(unlink(file.path(dir, '*.csv')))

    expect_identical(alternate_filename(filename),
                     normalizePath(filename, mustWork=FALSE))

    lastfilename <- filename
    for(i in 1:3) {
        fcon <- file(lastfilename, 'w')
        close(fcon)
        lastfilename <- alternate_filename(filename)
        expect_identical(lastfilename,
                         normalizePath(file.path(dir,sprintf('xyz%03d.csv',i)),
                                       mustWork=FALSE))
    }
})

test_that('alternate_filename does the right thing with xyz.abc.csv.', {
    dir <- tempdir()
    filename <- file.path(dir, 'xyz.abc.csv')
    if(file.exists(filename))
        unlink(filename)
    on.exit(unlink(file.path(dir, '*.csv')))

    expect_identical(alternate_filename(filename),
                     normalizePath(filename, mustWork=FALSE))

    lastfilename <- filename
    for(i in 1:3) {
        fcon <- file(lastfilename, 'w')
        close(fcon)
        lastfilename <- alternate_filename(filename)
        expect_identical(lastfilename,
                         normalizePath(file.path(dir,sprintf('xyz.abc%03d.csv',i)),
                                       mustWork=FALSE))
    }
})

test_that('alternate_filename does the right thing with no file extension.', {
    dir <- tempdir()
    filename <- file.path(dir, 'xyz')
    if(file.exists(filename))
        unlink(filename)
    on.exit(unlink(file.path(dir, '*.csv')))

    expect_identical(alternate_filename(filename),
                     normalizePath(filename, mustWork=FALSE))

    lastfilename <- filename
    for(i in 1:3) {
        fcon <- file(lastfilename, 'w')
        close(fcon)
        lastfilename <- alternate_filename(filename)
        expect_identical(lastfilename,
                         normalizePath(file.path(dir,sprintf('xyz%03d',i)),
                                       mustWork=FALSE))
    }
})

test_that('csv output works for separate tabs mode.', {
    ## unmerged version
    dir <- tempdir()
    varmat <- sapply(names(rslt), function(scen) {
                              sapply(names(rslt[[scen]]), function(var) {
                                              paste(scen, var, sep='.')
                                          })
                          })
    vlist <- as.vector(varmat)
    flist <- file.path(dir, paste0(vlist, '.csv'))
    on.exit(unlink(file.path(dir,'*.csv')))

    output(rslt, 'tabs', 'CSV', dir)

    for(i in seq_along(vlist)) {
        file <- flist[i]
        var <- vlist[i]
        expect_true(file.exists(file),
                    info=paste('Output file ', file, ' does not exist.'))
        df <- readr::read_csv(file)
        splt <- unlist(strsplit(var,'.', fixed=TRUE))
        s <- splt[1]
        v <- splt[2]
        expect_equal(df, rslt[[s]][[v]])
    }

    ## merged version
    vlist <- names(rsltmrg)
    flist <- file.path(dir, paste0(vlist, '.csv'))

    output(rsltmrg, 'tabs', 'CSV', dir)

    for(i in seq_along(vlist))  {
        file <- flist[i]
        var <- vlist[i]
        expect_true(file.exists(file),
                    info=paste('Output file ', file, ' does not exist.'))
        df <- readr::read_csv(file)
        expect_equal(df, rsltmrg[[var]])
    }
})

test_that('csv output works for single tab mode.',
      {
    dir <- tempdir()

    ## unmerged
    filename <- file.path(dir, 'iamrpt.csv')
    on.exit(unlink(file.path(dir, '*.csv')))
    output(rslt, 'merged', 'CSV', dir)
    expect_true(file.exists(filename))

    ## spot check a few lines in the data
    data <- readLines(filename)
    expect_equal(length(data), 6353)
    expect_identical(data[1], 'Reference.population')
    expect_identical(data[2], 'Units,scenario,region,year,value')
    expect_identical(data[100], 'thous,Reference,Argentina,2040,48142')
    expect_identical(data[707], '')
    expect_identical(data[708], 'Reference.floorspace')
    expect_identical(data[709],
                     'Units,scenario,region,building,nodeinput,building-node-input,year,value')
    expect_identical(data[1000],
                     'billion m^2,Reference,Brazil,resid,resid,resid_building,2015,3.60076')
    expect_identical(data[2119], 'Experiment 1.population')

    ## merged version
    filename <- file.path(dir, 'iamrpt001.csv')
    output(rsltmrg, 'merged', 'CSV', dir)
    expect_true(file.exists(filename))

    ## spot check important lines
    data <- readLines(filename)
    expect_equal(length(data), 6341)
    expect_identical(data[1], 'population')
    expect_identical(data[2], 'Units,scenario,region,year,value')
    expect_identical(data[100], 'thous,Reference,Argentina,2040,48142')
    expect_identical(data[707], 'thous,Experiment 1,Africa_Eastern,1975,98152')
    expect_identical(data[1500], 'thous,Experiment 2,Argentina,1990,32642')
    expect_identical(data[2115], '')
    expect_identical(data[2116], 'floorspace')
    expect_identical(data[2117],
                     'Units,scenario,region,building,nodeinput,building-node-input,year,value')
    expect_identical(data[2200],
                     'billion m^2,Reference,Africa_Northern,resid,resid,resid_building,2075,5.3585')

})


test_that('Single table can be converted to iiasa format.', {
    pop <- dplyr::filter(popq, year >= 2005, year <= 2020)
    popiia <- proc_var_iiasa(pop) %>%
      dplyr:::mutate(Variable='Population', Model='GCAM') %>%
      complete_iiasa_template(NULL) %>% # should be a no-op with no template.
      iiasa_sortcols()

    expect_equal(nrow(popiia), length(unique(pop$region)))
    expect_identical(names(popiia), c('Model', 'Scenario', 'Region', 'Variable',
                                      'Unit', as.character(seq(2005,2020,5))))
    expect_equal(popiia[['2010']], dplyr::filter(pop, year==2010)[['value']])
})

test_that('List of tables can be converted to iiasa format.', {
    pop <- dplyr::filter(popq, year >= 2005, year <= 2020)
    flrspc <-
        dplyr::filter(flrspcq, year >= 2005, year <= 2020) %>%
          aggregate('sum', 'scenario, region')

    allvar <- list(Population=pop, Floorspace=flrspc)

    iitbl <- iiasafy(allvar) %>%
      dplyr::mutate(Model='GCAM') %>%
      complete_iiasa_template(NULL) %>%
      iiasa_sortcols()

    expect_true(is.data.frame(iitbl))
    expect_equal(nrow(iitbl), 2*length(unique(pop$region)))
    expect_identical(names(iitbl), c('Model', 'Scenario', 'Region', 'Variable',
                                     'Unit', as.character(seq(2005,2020,5))))

    expect_identical(unique(iitbl$Variable), c('Population', 'Floorspace'))

})


test_that('IIASA template fills in missing rows for single table.', {
    tdata <- read_iiasa_template('test-data/iiasa-template.csv')
    pop <- dplyr::filter(popq, year >= 2005, year <= 2020)
    popiia <- proc_var_iiasa(pop) %>%
      dplyr:::mutate(Variable='Population', Model='GCAM') %>%
      complete_iiasa_template(tdata) %>%
      iiasa_sortcols()

    expect_equal(nrow(popiia), 2 * (1 + length(unique(pop$region))))
    expect_identical(names(popiia), c('Model', 'Scenario', 'Region', 'Variable',
                                      'Unit', as.character(seq(2005,2020,5))))
    expect_equal(popiia[ !is.na(popiia[['2010']]), ][['2010']],
                 dplyr::filter(pop, year==2010)[['value']])

    filled.rgn <- dplyr::filter(popiia, Region=='Wakanda')
    expect_equal(filled.rgn$Variable, c('Floorspace', 'Population'))
    expect_true(all(is.na(filled.rgn[['2010']])))
    expect_false(any(is.na(popiia$Model)))
    expect_false(any(is.na(popiia$Scenario)))
    expect_false(any(is.na(popiia$Region)))
})

test_that('IIASA template fills in missing rows for list of tables.', {
    tdata <- read_iiasa_template('test-data/iiasa-template.csv')
    pop <- dplyr::filter(popq, year >= 2005, year <= 2020)
    flrspc <-
        dplyr::filter(flrspcq, year >= 2005, year <= 2020) %>%
          aggregate('sum', 'scenario, region')

    allvar <- list(Population=pop, Floorspace=flrspc)

    iitbl <- iiasafy(allvar) %>%
      dplyr::mutate(Model='GCAM') %>%
      complete_iiasa_template(tdata) %>%
      iiasa_sortcols()

    expect_true(is.data.frame(iitbl))
    expect_equal(nrow(iitbl), 2*(1+length(unique(pop$region))))
    expect_identical(names(iitbl), c('Model', 'Scenario', 'Region', 'Variable',
                                     'Unit', as.character(seq(2005,2020,5))))

    ## The template has floorspace ahead of population, so it reverses the order.
    expect_identical(unique(iitbl$Variable), c('Floorspace', 'Population'))
    expect_false(any(is.na(iitbl$Model)))
    expect_false(any(is.na(iitbl$Scenario)))
    expect_false(any(is.na(iitbl$Region)))
})

test_that('xlsx output works for separate tabs mode.', {
    ## unmerged scenarios version
    dir <- tempdir()
    varmat <- sapply(names(rslt), function(scen) {
                              sapply(names(rslt[[scen]]), function(var) {
                                              paste(scen, var, sep='.')
                                          })
                          })
    vlist <- as.vector(varmat)
    filename <- file.path(dir, 'iamrpt.xlsx')
    on.exit(unlink(file.path(dir, '*.xlsx')))

    output(rslt, 'tabs', 'XLSX', dir)

    expect_true(file.exists(filename),
                info=paste('Output file ', filename, ' does not exist.'))
    wb <- openxlsx::loadWorkbook(filename)

    for(var in vlist) {
        df <- openxlsx::readWorkbook(wb, var)
        splt <- unlist(strsplit(var,'.', fixed=TRUE))
        s <- splt[1]
        v <- splt[2]
        expect_equivalent(df, rslt[[s]][[v]])
    }

    ## merged scenarios version
    vlist <- names(rsltmrg)
    filename <- file.path(dir, 'iamrpt001.xlsx')

    output(rsltmrg, 'tabs', 'XLSX', dir)

    expect_true(file.exists(filename),
                info=paste('Output file ', filename, ' does not exist.'))
    wb <- openxlsx::loadWorkbook(filename)

    for(var in vlist)  {
        df <- openxlsx::readWorkbook(wb, var)
        expect_equivalent(df, rsltmrg[[var]])
    }
})


test_that('xlsx output works in merged output mode.',
      {
    ## unmerged scenarios version
    dir <- tempdir()
    varmat <- sapply(names(rslt), function(scen) {
                              sapply(names(rslt[[scen]]), function(var) {
                                              paste(scen, var, sep='.')
                                          })
                          })
    vlist <- as.vector(varmat)
    filename <- file.path(dir, 'iamrpt.xlsx')
    on.exit(unlink(file.path(dir,'*.xlsx')))

    ## convert to wide format, which will make doing these tests so much
    ## easier.
    rsltw <- rslt
    for(scen in names(rslt)) {
        rsltw[[scen]] <-
            lapply(rsltw[[scen]],
                   function(df) {tidyr::spread(df, year, value)})
    }

    output(rsltw, 'merged', 'XLSX', dir)

    expect_true(file.exists(filename),
                info=paste('Output file ', filename, ' does not exist.'))

    wb <- openxlsx::loadWorkbook(filename)

    ## spot check some results
    r1 <- openxlsx::readWorkbook(wb, rows=1, colNames=FALSE)
    expect_identical(r1[1,1], 'Reference.population')
    r2 <- openxlsx::readWorkbook(wb, rows=(2:34))
    expect_equivalent(r2, rsltw$Reference$population)

    r1 <- openxlsx::readWorkbook(wb, rows=36, colNames=FALSE)
    expect_identical(r1[1,1], 'Reference.floorspace')
    r2 <- openxlsx::readWorkbook(wb, rows=(37:101))
    expect_equivalent(r2, rsltw$Reference$floorspace)

    r1 <- openxlsx::readWorkbook(wb, rows=240, colNames=FALSE)
    expect_identical(r1[1,1], 'Experiment 2.floorspace')
    r2 <- openxlsx::readWorkbook(wb, startRow=241)
    expect_equivalent(r2, rsltw[['Experiment 2']]$floorspace)

    ## merged scenarios version
    vlist <- names(rsltmrg)
    filename <- file.path(dir, 'iamrpt001.xlsx')

    rsltmw <- lapply(rsltmrg, function(df) {tidyr::spread(df, year, value)})
    output(rsltmw, 'merged', 'XLSX', dir)
    expect_true(file.exists(filename))

    wb <- openxlsx::loadWorkbook(filename)

    r1 <- openxlsx::readWorkbook(wb, rows=1, colNames=FALSE)
    expect_identical(r1[1,1], 'population')
    r2 <- openxlsx::readWorkbook(wb, rows=(2:98))
    expect_equivalent(r2, rsltmw$population)

    r1 <- openxlsx::readWorkbook(wb, rows=100, colNames=FALSE)
    expect_identical(r1[1,1], 'floorspace')
    r2 <- openxlsx::readWorkbook(wb, startRow=101)
    expect_equivalent(r2, rsltmw$floorspace)

})

