context('Aggregation function')

library(dplyr, quietly=TRUE, warn.conflicts=FALSE)

load('test-data/flrspcq.rda')


test_that('Aggregate with no keys returns unmodified table.', {
    df <- aggregate(flrspcq, 'sum', NA)
    expect_identical(df, flrspcq)
})


test_that('Can aggregate with one key.', {
    df <- aggregate(flrspcq, NA, 'region')
    dfq <- group_by(flrspcq, scenario, region, year, Units) %>%
      summarise(value=sum(value)) %>%
      ungroup

    expect_equal(df, dfq)
})


test_that('Can aggregate with multiple keys.', {
    df <- aggregate(flrspcq, NA, 'region, building')
    dfq <- group_by(flrspcq, scenario, region, year, building, Units) %>%
      summarise(value=sum(value)) %>%
      ungroup

    expect_equal(df, dfq)
})


test_that('Can specify aggregation function.', {
    dfq <- group_by(flrspcq, scenario, year, building, Units)

    df1 <- aggregate(flrspcq, 'sum', 'building')
    dfq1 <- summarise(dfq, value=sum(value)) %>% ungroup
    expect_equal(df1, dfq1)

    df2 <- aggregate(flrspcq, 'mean', 'building')
    dfq2 <- summarise(dfq, value=mean(value)) %>% ungroup

    df3 <- aggregate(flrspcq, 'max', 'building')
    dfq3 <- summarise(dfq, value=max(value)) %>% ungroup

    df4 <- aggregate(flrspcq, 'min', 'building')
    dfq4 <- summarise(dfq, value=min(value)) %>% ungroup

    df5 <- aggregate(flrspcq, 'median', 'building')
    dfq5 <- summarise(dfq, value=median(value)) %>% ungroup
})

test_that('System handles warning bogus aggregation keys', {
    expect_warning({df <- aggregate(flrspcq, 'sum', 'region, sector')},
                   'not found in variable data')
    dfq <- group_by(flrspcq, scenario, region, year, Units) %>%
      summarise(value=sum(value)) %>%
      ungroup

    expect_equal(df, dfq)
})

test_that('System handles bogus aggregation functions',
      {
    expect_warning({df <- aggregate(flrspcq, 'frobnitz', 'region, sector')},
                   'not found in allowed aggregation function table')
    expect_equal(df, flrspcq)
})
