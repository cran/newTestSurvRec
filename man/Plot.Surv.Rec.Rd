\name{Plot.Surv.Rec}
\alias{Plot.Surv.Rec}

\title{Plots of thesurvival function from an object with class newTestSurvRec, using PHS or WC models}

\description{The survival curves are plotted. Both curves are estimates using PSH o WC estimator. This package is available in language R. This important clearly, that the PHS estimator is of valid use when it assumed that the inter-occurrence times are  IID. Its obvious that this assumption is restrictive in biomedical applications and its use is more valid on the field of engineering. For WC estimated not import if the data is correlated.}

\usage{Plot.Surv.Rec(XX,...)}

\arguments{
\item{XX}{Data type recurrent events. Example: TBCplapyr}
\item{...}{Other objects}
}

\value{The survival curves for both groups are plotted.}

\references{\bold{Martinez C., Ramirez, G., Vasquez M.} (\bold{2009}).Pruebas no parametricas para comparar curvas de supervivencia de dos grupos que experimentan eventos recurrentes. Propuestas. \emph{Revista Ingenieria U.C.},Vol 16, 3, 45-55.// \bold{Pena E., Strawderman R., Hollander M.} (\bold{2001}). Nonparametric Estimation with Recurrent Event Data. \emph{J.A.S.A.} 96, 1299-1315.}

\author{Dr. \bold{Carlos M. Martinez M.} <cmmm7031@gmail.com>}

\seealso{Plot.Event.Rec, Dif.Surv.Rec}

\examples{XL<-data(TBCplapyr)
Plot.Surv.Rec(TBCplapyr)}

\keyword{Plot}
\keyword{Survival}