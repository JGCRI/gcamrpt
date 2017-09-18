context('runModule and friends')

test_that('Modules can be listed.', {
    ml <- listModules()
    expect_true(is.character(ml))
    expect_true('test_fun' %in% ml)
    expect_true('population' %in% ml)
})

test_that('Test module can be run through the generic interface.', {
    rq <- runModule('test_fun', GETQ)
    expect_identical(rq, 'Test Query')

    expect_message({df <- runModule('test_fun', RUN)}, 'Test Module')
    expect_equal(nrow(df), 0)
})

test_that('Test module gets found with non-canonical case.', {
    rq <- runModule('TeSt_FuN', GETQ)
    expect_identical(rq, 'Test Query')

    expect_message({df <- runModule('TeSt_FUn', RUN)}, 'Test Module')
    expect_equal(nrow(df), 0)
})
