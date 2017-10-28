prep.emissionsconv <- function()
{
    fromC <-  c(1,        44/12)
    fromCO2 <- c(12/44, 1     )
    cm <- matrix(c(fromC, fromCO2), nrow=2, byrow=TRUE)
    colnames(cm) <- c('C', 'CO2')
    row.names(cm) <- colnames(cm)

    cm
}
