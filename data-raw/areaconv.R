prep.areaconv <- function()
{
    ## Recognized units are:
    ## mm2, cm2, meter2, km2, mi2, ft2
    ## Note that having one unit that is a substring of another is a problem, so
    ## we use 'meter' instead of m
    ##           mm2       cm2       meter2    km2        mi2        ft2
    frommm2 <- c(1,        0.01,     1e-6,     1e-12,     3.861e-13, 1.0764e-5)
    fromcm2 <- c(100,      1,        1e-4,     1e-10,     3.861e-11, 1.0764e-3)
    frommeter2 <- c(1e6,   1e3,      1,        1e-6,      3.861e-7,  1.0764e1)
    fromkm2 <- c(1e12,     1e10,     1e6,      1,         3.861e-1,  10.764e7)
    frommi2 <- c(2.59e12,  2.59e+10, 2.59e+6,  2.59,      1,         2.788e+7)
    fromft2 <- c(9.2903e4, 9.2903e2, 0.092903, 9.2903e-8, 3.587e-8,  1)
    m <- matrix(c(frommm2, fromcm2, frommeter2, fromkm2, frommi2, fromft2),
                ncol=6, byrow=TRUE)
    colnames(m) <- c('mm2', 'cm2', 'meter2', 'km2', 'mi2', 'ft2')
    row.names(m) <- colnames(m)

    m
}
