% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/units.R
\name{parsecounts}
\alias{parsecounts}
\title{Parse counting units (thousands, millions, etc) from strings}
\usage{
parsecounts(unit)
}
\arguments{
\item{unit}{The counting unit to convert.}
}
\description{
Counting units tend to get treated as words rather than symbols, and thus
have a lot of variant forms.  Here, we parse these with regular expressions
that should catch the most common ones.  When we find a unit, we return the
conversion factor to convert it to ones.  For example, "thousands" -> 0.001.
This allows us to construct the conversion from unit 'a' to unit 'b' by
dividing val(b)/val(a).  Note that an empty input string is treated as ones
(i.e., if you don't specify a count, it's assumed to be ones).
}
\details{
If we can't identify a unit, then we issue a warning and return NA.  This
will cause \code{\link{unitconv_counts}} to abort the unit conversion.

Right now we cover all of the units I've seen in the GCAM output, plus a few
other obvious extensions, but there are many more possibilities.  The units
are matched by case-insensitive regular expression; the matches currently
implemented are:

\describe{
  \item{(blank string)}{Ones}
  \item{\code{hundred[ -]?thous}}{Hundred thousands}
  \item{\code{hundred}}{Hundreds (the compound 'hundred thousands' is checked
  first so that this match will not capture it.)}
  \item{\code{thous}}{Thousands (including abbreviations like 'thous' or
  'thousand')}
  \item{\code{mil}}{Millions, including abbreviations}
  \item{\code{bil}}{Billions, including abbreviations}
  \item{\code{tril}}{Trillions, including abbreviations.  This is the largest
  unit we expect to see written out (instead of denoted with a metric prefix).}
}
}
