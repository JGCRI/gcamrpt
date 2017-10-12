## Prepare package data for energy unit conversion.
## Usage: to <- from * energyconv[fromunit, tounit]
##
## Conversion factors from larger units to smaller units are good to 3
## significant figures.  The inverse conversions are computed as c[x,y] <-
## 1/c[y,x] to ensure that converting forward and back gives you something
## reasonably close to what you started with.

prep.energyconv <- function()
{

    fromEJ <-  c(1          , 1e6    , 1e12  , 2.78e8     ) #1e18 J
    fromTJ <-  c(1e-6       , 1      , 1e6   , 1/(3.6e-3) ) #1e12 J
    fromMJ <-  c(1e-12      , 1e-6   , 1     , 1/(3.6e3)  ) #1e6 J
    fromMWh <- c(1/(2.78e8) , 3.6e-3 , 3.6e3 , 1          )
    cm <- matrix(c(fromEJ, fromTJ, fromMJ, fromMWh), nrow=4, byrow=TRUE)
    colnames(cm) <- c('ej', 'tj', 'mj', 'mwh')
    row.names(cm) <- colnames(cm)

    cm
}
