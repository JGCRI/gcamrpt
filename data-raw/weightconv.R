prep.weightconv <- function()
{
    frommg <-  c(1,        1e-3)
    fromkt <- c(1e3, 1     )
    cm <- matrix(c(frommg, fromkt), nrow=2, byrow=TRUE)
    colnames(cm) <- c('mg', 'kt')
    row.names(cm) <- colnames(cm)

    cm
}
