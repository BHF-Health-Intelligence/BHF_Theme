% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bhf_theme.R
\name{scale_fill_bhf}
\alias{scale_fill_bhf}
\title{BHF Colour Fill}
\usage{
scale_fill_bhf(palette = "reds", discrete = TRUE, reverse = FALSE, ...)
}
\arguments{
\item{palette}{Colour palette - defaults to reds}

\item{discrete}{Ensures the scale is discrete/continuous}

\item{reverse}{Reverses the palette order}
}
\value{
Colour fill object
}
\description{
Get a BHF colour fill
}
\examples{
scale <- scale_fill_bhf("reds",reverse=TRUE)
}
