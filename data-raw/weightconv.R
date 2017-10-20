prep.weightconv <- function()
{
    ## Recognized units are:
    ## kg, Mg, Gg, Tg, Pg, kT, MT, tonne
    ## Note that having one unit that is a substring of another is a problem, so
    ## we use 'tonne' instead of T
    fromkg <- c(1,  1e-3, 1e-6, 1e-9, 1e-12, 1e-6, 1e-9, 1e-3)
    fromMg <- c(1e3, 1,   1e-3, 1e-6, 1e-9,  1e-3, 1e-6, 1)
    fromGg <- c(1e6, 1e3, 1,    1e-3, 1e-6, 1,   1e-3,    1e3)
    fromTg <- c(1e9, 1e6, 1e3,  1,    1e-3, 1e3,   1,  1e6)
    fromPg <- c(1e12, 1e9, 1e6, 1e3,  1,    1e6,   1e3,  1e9)
    fromkT <- c(1e6, 1e3, 1,    1e-3, 1e-6, 1,   1e-3,    1e3)
    fromMT <- c(1e9, 1e6, 1e3,  1,    1e-3, 1e3,   1,  1e6)
    fromtonne <- c(1e3, 1,   1e-3, 1e-6, 1e-9,  1e-3, 1e-6, 1)
    cm <- matrix(c(fromkg, fromMg, fromGg, fromTg, fromPg, fromkT, fromMT,
                   fromtonne), ncol=8, byrow=TRUE)
    colnames(cm) <- c('kg', 'Mg', 'Gg', 'Tg', 'Pg', 'kT', 'MT', 'tonne')
    row.names(cm) <- colnames(cm)

    cm
}
