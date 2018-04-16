\name{Qsearch.Fractil}
\alias{Qsearch.Fractil}

\title{Calculate the survival time to a selected quantile}

\description{Auxiliary function called from Dif.Surv.Rec function. Given a FitSurvRec object we obtain the quantile from a survival function using PHS o WC estimators.}

\usage{Qsearch.Fractil(fr, qr = 0.5)}

\arguments{
  \item{fr}{FitSurvRec object}
  \item{qr}{quantile. Default is 0.5}
}

\value{Returns the time in a selected quantile}

\references{\bold{Martinez C., Ramirez, G., Vasquez M.} (\bold{2009}). Pruebas no parametricas para comparar curvas de supervivencia de dos grupos  que experimentan eventos recurrentes. Propuestas. \emph{Revista Ingenieria U.C.},Vol 16, 3, 45-55.// \bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de naturaleza recurrente. Tesis doctoral.\emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.}

\author{Dr. \bold{Carlos M Martinez M.}, <cmmm7031@gmail.com>}

\seealso{FitSurvRe, Survrecu, is.Survrecu}

\examples{XL<-data(MMC.TestSurvRec)
fit<-FitSurvRec(Survrecu(id,time,event)~1,data=MMC.TestSurvRec)
# 35th percentile from the survival function
Qsearch.Fractil(fit,q=0.35)
	 }

\keyword{Quantile}
\keyword{Survival}
