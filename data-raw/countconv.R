prep.countconv <- function()
{
    fromBillion  <- c(1     , 1e3    , 1e6   , 1e9 )
    fromMillion  <- c(1/1e3 , 1      , 1e3   , 1e6 )
    fromThousand <- c(1/1e6 , 1/1e3  , 1     , 1e3 )
    fromUnity    <- c(1/1e9 , 1/1e6  , 1/1e3 , 1   )
    cm <- matrix(c(fromBillion, fromMillion, fromThousand, fromUnity), nrow=4, byrow=TRUE)
    colnames(cm) <- c('billion', 'million', 'thousand', 'unity')
    row.names(cm) <- colnames(cm)

    cm
}
