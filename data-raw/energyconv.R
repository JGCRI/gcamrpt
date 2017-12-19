## Prepare package data for energy unit conversion.
## Usage: to <- from * energyconv[fromunit, tounit]
##
## Conversion factors from larger units to smaller units are good to 3
## significant figures.  The inverse conversions are computed as c[x,y] <-
## 1/c[y,x] to ensure that converting forward and back gives you something
## reasonably close to what you started with.

prep.energyconv <- function()
{

    #           | EJ      | TJ     | MJ     | MWh     | TWh     | Mboe      | Pcal      |
    #           |---------|--------|--------|---------|---------|-----------|-----------|
    fromEJ <-   c(1       , 1e6    , 1e12   , 1/3.6e-9, 1/3.6e-3, 1/6.118e-3, 2.390e2   ) #1e18 J
    fromTJ <-   c(1e-6    , 1      , 1e6    , 1/3.6e-3, 1/3.6e3 , 1/6.118e3 , 2.390e-04 ) #1e12 J
    fromMJ <-   c(1e-12   , 1e-6   , 1      , 1/3.6e3 , 1/3.6e9 , 1/6.118e9 , 2.390e-10 ) #1e6 J
    fromMWh <-  c(3.6e-9  , 3.6e-3 , 3.6e3  , 1       , 1e-6    , 1/1.699e6 , 8.606e-07 )
    fromTWh <-  c(3.6e-3  , 3.6e3  , 3.6e9  , 1e6     , 1       , 1/1.699   , 8.606e-01 )
    fromMboe <- c(6.118e-3, 6.118e3, 6.118e9, 1.699e6 , 1.699   , 1         , 1.400     )
    fromPcal <- c(4.184e-3, 4.184e3, 4.184e9, 1.162e6 , 1.162   , 7.143e-1  , 1         )

    cm <- matrix(c(fromEJ, fromTJ, fromMJ, fromMWh, fromTWh, fromMboe, fromPcal), nrow=7, byrow=TRUE)
    colnames(cm) <- c('EJ', 'TJ', 'MJ', 'MWh', 'TWh', 'Mboe', 'Pcal')
    row.names(cm) <- colnames(cm)

    cm
}
