prep.emissionsconv <- function()
{
    fromC <-  c(1,        44/12,       44/12)
    fromCO2 <- c(12/44,       1,           1)
    fromCO2e <- c(12/44,      1,           1)
    cm <- matrix(c(fromC, fromCO2, fromCO2e), nrow=3, byrow=TRUE)
    colnames(cm) <- c('C', 'CO2', 'CO2e')
    row.names(cm) <- colnames(cm)

    cm
}
