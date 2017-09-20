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

test_that('output_csv works for separate tabs mode.', {
    ## unmerged version
    dir <- tempdir()
    varmat <- sapply(names(rslt), function(scen) {
                              sapply(names(rslt[[scen]]), function(var) {
                                              paste(scen, var, sep='.')
                                          })
                          })
    vlist <- as.vector(varmat)
    flist <- file.path(dir, paste0(vlist, '.csv'))
    on.exit(unlink(flist))

    output_csv(rslt, 'tabs', dir)

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
    on.exit(unlink(flist), add=TRUE)

    output_csv(rsltmrg, 'tabs', dir)

    for(i in seq_along(vlist))  {
        file <- flist[i]
        var <- vlist[i]
        expect_true(file.exists(file),
                    info=paste('Output file ', file, ' does not exist.'))
        df <- readr::read_csv(file)
        expect_equal(df, rsltmrg[[var]])
    }
})

test_that('output_csv works for single tab mode.',
      {
    dir <- tempdir()

    ## unmerged
    filename <- file.path(dir, 'iamrpt.csv')
    on.exit(unlink(filename))
    output_csv(rslt, 'merged', dir)
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
    on.exit(unlink(filename), add=TRUE)
    output_csv(rsltmrg, 'merged', dir)
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
