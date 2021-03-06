% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XPSCustomBaseline.r
\name{LPShirley}
\alias{LPShirley}
\title{LPShirley is a LinearPolynomial * Shirley Baseline see Practical Surface Analysis Briggs and Seah Wiley Ed.}
\usage{
LPShirley(object, m, limits)
}
\arguments{
\item{object}{coreline region where to apply LPshirley function}

\item{m}{coefficient to}

\item{limits}{limits of the XPSSample object}
}
\value{
returns the LPShirley background
}
\description{
Applies in all the cases where the Shirley background crosses the spectrum
causing the Shirley algorith to diverge. 
In LPShirley firstly a linear background subtraction is performed to recognize presence of multiple peaks
Modified Bishop polynom weakens the Shirley cross section
Bishop polynom:  PP[ii]<-1-m*ii*abs(dX)   dX = energy step, m=coeff manually selected
in LPShirley the classical Shirley expression is multiplied by PP:
S(Ei) =  Shirley(E) = Integral( L(E)*K(E'-E)* PP(E)* J(E)dE'     J(E) = acquired spectrum
where the integral is extended between Ei and infinite and j=i-1
}
