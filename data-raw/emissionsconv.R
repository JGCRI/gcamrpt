prep.emissionsconv <- function()
{
    fromMTC <-  c(1,        42/12)
    fromMTCO2 <- c(12/42, 1     )
    cm <- matrix(c(fromMTC, fromMTCO2), nrow=2, byrow=TRUE)
    colnames(cm) <- c('mtc', 'mtco2')
    row.names(cm) <- colnames(cm)

    cm
}
