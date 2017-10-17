prep.emissionsconv <- function()
{
    fromMTC <-  c(1,        44/12)
    fromMTCO2 <- c(12/44, 1     )
    cm <- matrix(c(fromMTC, fromMTCO2), nrow=2, byrow=TRUE)
    colnames(cm) <- c('mtc', 'mtco2')
    row.names(cm) <- colnames(cm)

    cm
}
