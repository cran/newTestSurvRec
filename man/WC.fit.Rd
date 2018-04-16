\name{WC.fit}
\alias{WC.fit}

\title{Survival function estimator for recurrence time data using the estimator developed by Wang and Chang}

\description{Estimation of survival function for correlated by the product limit estimator PLE method developed by Wang and Chang.}

\usage{WC.fit(x, tvals)}

\arguments{
  \item{x}{a survival recurrent event object}
  \item{tvals}{vector of times where the survival function can be estimated.}
          }

\details{Wang-Chang (1999) proposed an estimator of the common marginal survivor function in the case where within-unit inter-occurrence times are correlated. The correlation structure considered by Wang and Chang (1999) is quite general and contains, in particular, both the i.i.d. and multiplicative (hence gamma) frailty model as special cases. This estimator removes the bias noted for the product-limit estimator developed by Pena, Strawderman and Hollander (PSH, 2001) when inter-occurrence times are correlated within units. However, when applied to i.i.d. inter-occurrence times, this estimator is not expected to perform as well as the PSH estimator, especially with regard to efficiency.}

\value{Value returned
\item{n}{number of unit or subjects observed.}
\item{m}{vector of number of recurrences in each subject (length n)}
\item{failed}{vector of number of recurrences in each subject (length n*m). Vector ordered (e.g. times of first unit, times of second unit, ..., times of n-unit)}
\item{censored}{vector of times of censorship for each subject (length n)}
\item{numdistinct}{number of distinct failures times.}
\item{distinct}{vector of distinct failures times.}
\item{AtRisk}{matrix of number of persons-at-risk at each distinct time and for each subject}
\item{survfunc}{vector of survival estimated in distinct times}
\item{tvals}{copy of argument.}
      }

\references{\bold{Wang, M. C. and Chang, S.H.} (\bold{1999}). Nonparametric Estimation of a Recurrent Survival Function. \emph{J. Amer. Statist. Assoc.} 94, 146-153.// \bold{Pena E., Strawderman R., Hollander M.} (\bold{2001}). Nonparametric Estimation with Recurrent
Event Data. \emph{J.A.S.A.} 96, 1299-1315.}

\author{Dr. \bold{Carlos M. Martinez M.}, <cmmm7031@gmail.com>}

\note{This function was originally performed by the survrec package, which solved the adjustment problem of the WC estimator using Fortran routines. With the permission of its author, the algorithm was taken, modified, the algorithm, WC estimator was reprogrammed and adapted to the needs of the newTestSurvRec package and thus avoid dependence.}

\seealso{PSH.fit, Plot.Event.Rec, Plot.Surv.Rec, Print.Summary}

\examples{XL<-data(MMC.TestSurvRec)
#-------------------------------------------------------------------------------------
fitPSHa<-PSH.fit(Survrecu(MMC.TestSurvRec$id,MMC.TestSurvRec$time,
                        MMC.TestSurvRec$event))
fitPSHa$surv
fitPSHa$time
plot(fitPSHa$time,fitPSHa$survfunc,type="s" ,ylim=c(0,1),
        xlim=c(0,max(fitPSHa$time)))
title(main = list("Survival Curve with Recurrent Event Data", 
        cex = 0.8, font = 2.3, col = "dark blue"))
mtext("Research Group: AVANCE USE R!", cex = 0.7, font = 2, 
        col = "dark blue", line = 1)
mtext("Software made by: Dr. Carlos Martinez", cex = 0.6, font = 2, 
        col = "dark red", line = 0)

\donttest{
fitWCa<-WC.fit(Survrecu(MMC.TestSurvRec$id,MMC.TestSurvRec$time,
                        MMC.TestSurvRec$event))
fitWCa$surv
fitWCa$time
plot(fitWCa$time,fitWCa$survfunc,type="s" ,ylim=c(0,1),
        xlim=c(0,max(fitWCa$time)))
}
}

\keyword{datasets}
\keyword{survival}
