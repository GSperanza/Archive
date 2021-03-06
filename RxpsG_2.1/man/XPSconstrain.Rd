% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XPSConstrain.r
\name{XPSconstrain}
\alias{XPSconstrain}
\title{XPSConstrain function to set fit constraints}
\usage{
XPSconstrain(Object, ncomponent = NA, action = c("show", "link", "fix",
  "set", "remove"), variable = NULL, parameter = NULL, value = NA,
  expr)
}
\arguments{
\item{Object}{XPSSample}

\item{ncomponent}{the fit component index}

\item{action}{link, fix, set, remove the constraint to apply or remove}

\item{variable}{h, mu, sigma, etc.   the fit parameter to constrain}

\item{parameter}{one of the values start, min, max}

\item{value}{the value to set}

\item{expr}{the constraint expression}
}
\description{
XPSConstrain function to set fit constraints
}
\examples{
\dontrun{
XPSconstrain(XPSCoreline[[3]], ncomponent=2, action="link", variable="mu", expr="mu1+1.5")
XPSconstrain(XPSCoreline[[2]], ncomponent=3, action="link", variable="sigma", expr="sigma1")
}

}
