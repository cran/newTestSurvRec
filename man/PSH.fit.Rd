\name{PSH.fit}
\alias{PSH.fit}

\title{Estimator of the survival curve  using the estimator developed by Pena, Strawderman and Hollander}

\description{ Estimation of survival function for recurrence time data by means the generalized product limit estimator (PLE) method developed by Pena, Strawderman and Hollander. The resulting object of class Survrecu is plotted by plot, before it is returned.}

\usage{PSH.fit(x, tvals)}

\arguments{
  \item{x}{a survival recurrent event object}
  \item{tvals}{vector of times where the survival function can be estimated.}
          }

\details{The estimator computed by this object is the nonparametric estimator of the inter-event time survivor function under the assumption of a renewal or IID model. This generalizes the product-limit estimator to the situation where the event is recurrent. For details and the theory behind this estimator, please refer to Pena, Strawderman and Hollander (2001, JASA).}

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

\references{\bold{Pena, E.A., Strawderman, R. and Hollander  M.} (\bold{2001}). Nonparametric Estimation with Recurrent Event Data. \emph{J. Amer. Statist. Assoc.} 96, 1299-1315.// \bold{Pena E., Strawderman R., Hollander, M.} (\bold{2001}). Nonparametric Estimation with Recurrent Event Data. \emph{J.A.S.A.} 96, 1299-1315.}

\author{Dr. \bold{Carlos M. Martinez M.}, <cmmm7031@gmail.com>}

\note{This function was originally performed by the survrec package, which solved the adjustment problem of the PSH estimator using Fortran routines. With the permission of its author, the algorithm of the packet base was taken, modified, the algorithm of the PSH estimates was reprogrammed and adapted to the needs of the newTestSurvRec package and thus avoid dependence.}

\seealso{WC.fit, Survrecu, Plot.Event.Rec, Plot.Surv.Rec, Print.Summary}

\examples{data(MMC.TestSurvRec)
fitPSHa<-PSH.fit(Survrecu(MMC.TestSurvRec$id,MMC.TestSurvRec$time,
                          MMC.TestSurvRec$event))
fitPSHa$surv
fitPSHa$time
plot(fitPSHa$time,fitPSHa$survfunc,type="s" ,ylim=c(0,1),xlim=c(0,max(fitPSHa$time)))
  	  title(main = list("Survival Curve with Recurrent Event Data", 
        cex = 0.8, font = 2.3, col = "dark blue"))
mtext("Research Group: AVANCE USE R!", cex = 0.7, font = 2, 
        col = "dark blue", line = 1)
mtext("Software made by: Dr. Carlos Martinez", cex = 0.6, font = 2, 
        col = "dark red", line = 0)
}

\keyword{Statistical}
\keyword{Tests}
