## Prepare package data for energy unit conversion.
## Usage: to <- from * energyconv[fromunit, tounit]
##
## Conversion factors from larger units to smaller units are good to 3
## significant figures.  The inverse conversions are computed as c[x,y] <-
## 1/c[y,x] to ensure that converting forward and back gives you something
## reasonably close to what you started with.

prep.energyconv <- function()
{

    fromEJ <-   c(1          , 1e3      , 1e6      , 1e12    , 1/3.6e-9  , 1/3.6e-3 , 1/6.118e-3 , 1/6.118e-9 ) #1e18 J
    fromPJ <-   c(1e-3       , 1        , 1e3      , 1e9     , 1/3.6e-6  , 1/3.6    , 1/6.118    , 1/6.118e-6 )
    fromTJ <-   c(1e-6       , 1e-3     , 1        , 1e6     , 1/3.6e-3  , 1/3.6e3  , 1/6.118e3  , 1/6.118e-3 ) #1e12 J
    fromMJ <-   c(1e-12      , 1e-9     , 1e-6     , 1       , 1/3.6e3   , 1/3.6e9  , 1/6.118e9  , 1/6.118e3  ) #1e6 J
    fromMWh <-  c(3.6e-9     , 3.6e-6   , 3.6e-3   , 3.6e3   , 1         , 1e-6     , 1/1.699e6  , 1/1.1699   )
    fromTWh <-  c(3.6e-3     , 3.6      , 3.6e3    , 3.6e9   , 1e6       , 1        , 1/1.699    , 1/1.699e-6 )
    fromMboe <- c(6.118e-3   , 6.118    , 6.118e3  , 6.118e9 , 1.699e6   , 1.699    , 1          , 1e-6       )
    fromboe <-  c(6.118e-9   , 6.118e-6 , 6.118e-3 , 6.118e3 , 1.699     , 1.699e-6 , 1e6        , 1          )
    cm <- matrix(c(fromEJ, fromPJ, fromTJ, fromMJ, fromMWh, fromTWh, fromMboe, fromboe), nrow=8, byrow=TRUE)
    colnames(cm) <- c('EJ', 'PJ', 'TJ', 'MJ', 'MWh', 'TWh', 'Mboe', 'boe')
    row.names(cm) <- colnames(cm)

    cm
}
