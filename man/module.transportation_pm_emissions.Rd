% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transportation.R
\name{module.transportation_pm_emissions}
\alias{module.transportation_pm_emissions}
\title{Produce particulate matter emissions from transportation}
\usage{
module.transportation_pm_emissions(mode, allqueries, aggkeys, aggfn, years,
  filters, ounit)
}
\description{
Convert service output in passenger-km or tonne-km to vehicle-km using the
load factor table.  Then apply a table of emissions coefficients to get
emissions.  Note this doesn't actually use any emissions generated from
GCAM.
}
\keyword{internal}
