prep.countconv <- function()
{
    fromBillion   <- c(1     , 1e3    , 1e6   , 1e9 , 1e9 , 1e9 , 1e9, 1e9, 1e9)
    fromMillion   <- c(1/1e3 , 1      , 1e3   , 1e6 , 1e6 , 1e6 , 1e6, 1e6, 1e6)
    fromThousand  <- c(1/1e6 , 1/1e3  , 1     , 1e3 , 1e3 , 1e3 , 1e3, 1e3, 1e3)
    fromPerson    <- c(1/1e9 , 1/1e6  , 1/1e3 , 1   , 1   , 1   , 1  , 1, 1)
    fromVehicle   <- c(1/1e9 , 1/1e6  , 1/1e3 , 1   , 1   , 1   , 1  , 1, 1)
    fromM2        <- c(1/1e9 , 1/1e6  , 1/1e3 , 1   , 1   , 1   , 1  , 1, 1)
    fromKM        <- c(1/1e9 , 1/1e6  , 1/1e3 , 1   , 1   , 1   , 1  , 1, 1)
    fromTONKM     <- c(1/1e9 , 1/1e6  , 1/1e3 , 1   , 1   , 1   , 1  , 1, 1)
    fromVehicleKM <- c(1/1e9 , 1/1e6  , 1/1e3 , 1   , 1   , 1   , 1  , 1, 1)
    cm <- matrix(c(fromBillion, fromMillion, fromThousand, fromPerson, fromVehicle, fromM2, fromKM, fromTONKM, fromVehicleKM), nrow=9, byrow=TRUE)
    colnames(cm) <- c('billion', 'million', 'thousand', 'person', 'vehicle', 'm2', 'km', 'tonkm', 'vehiclekm')
    row.names(cm) <- colnames(cm)

    cm
}
