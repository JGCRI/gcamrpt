prep.countconv <- function()
{
    fromBillion  <- c(1     , 1e3    , 1e6   )
    fromMillion  <- c(1/1e3 , 1      , 1e3   )
    fromThousand <- c(1/1e6 , 1/1e3  , 1     )
    cm <- matrix(c(fromBillion, fromMillion, fromThousand), nrow=3, byrow=TRUE)
    colnames(cm) <- c('billion', 'million', 'thousand')
    row.names(cm) <- colnames(cm)

    cm
}
