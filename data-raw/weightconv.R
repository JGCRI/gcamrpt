prep.weightconv <- function()
{
    fromg <-  c(1,        1e-9)
    fromkt <- c(1e9, 1     )
    cm <- matrix(c(fromg, fromkt), nrow=2, byrow=TRUE)
    colnames(cm) <- c('g', 'kt')
    row.names(cm) <- colnames(cm)

    cm
}
