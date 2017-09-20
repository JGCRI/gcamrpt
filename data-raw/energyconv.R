## Prepare package data for energy unit conversion.
## Usage: to <- from * energyconv[fromunit, tounit]
##
## Conversion factors from larger units to smaller units are good to 3
## significant figures.  The inverse conversions are computed as c[x,y] <-
## 1/c[y,x] to ensure that converting forward and back gives you something
## reasonably close to what you started with.

prep.energyconv <- function()
{

    fromEJ <-  c(1,        2.78e8)
    fromMWh <- c(1/2.78e8, 1     )
    cm <- matrix(c(fromEJ, fromMWh), nrow=2, byrow=TRUE)
    colnames(cm) <- c('EJ', 'MWh')
    row.names(cm) <- colnames(cm)

    cm
}
