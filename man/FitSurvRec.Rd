\name{FitSurvRec}
\alias{FitSurvRec}

\title{Compute a Survival Curve for Recurrent Event Data given a variable of group}

\description{Computes an estimate of a survival curve for recurrent event data using either the \emph{Pena, Strawderman and  Hollander}or \emph{Wang and Chang} estimators. It also computes the asymptotic standard errors. The resulting object of class Survrecu is plotted.}

\usage{FitSurvRec(formula, data, type = "pena-strawderman-hollander", ...)}

\arguments{
\item{formula}{A formula object. If a formula object is supplied it must have a Survrecu object as the response on the left of the operatorand a term on the right. For a single survival curve as  part of the formula is required.}
\item{data}{a data frame in wich to interpret the variables named in the formula.}
\item{type}{a character string specifying the type of survival curve. Possible value are "pena- strawderman-hollander" or "wang-chang". The default is "pena,-strawderman-hollander".}
\item{\dots}{additional arguments passed to the type of estimator.}
}

\details{See the help details of PSH.fit or WC.fit depending on the type chosen}

\value{A FitSurvRec object. Methods defined for FitSurvRec objects are provided for print, lines and plot.}

\references{\bold{Martinez C., Ramirez, G., Vasquez M.} (\bold{2009}).Pruebas no parametricas para comparar curvas de supervivencia de dos grupos  que experimentan eventos recurrentes. Propuestas. \emph{Revista Ingenieria U.C.},Vol 16, 3, 45-55.//\bold{Pena E., Strawderman R., Hollander M. (2001)}. Nonparametric Estimation with Recurrent  Event Data. \emph{J.A.S.A.} 96, 1299-1315}

\author{Dr. \bold{Carlos M. Martinez M.}, <cmmm7031@gmail.com>}

\seealso{is.Survrecu, Survrecu, PSH.fit, Plot.Event.Rec, Plot.Surv.Rec, Print.Summary}

\examples{
data(MMC.TestSurvRec)
# fit a PSH survival function and plot it
fitPSH<-FitSurvRec(Survrecu(id,time,event)~1,data=MMC.TestSurvRec)
plot(fitPSH$time,fitPSH$survfunc,type="s" ,ylim=c(0,1),
     xlim=c(0,max(fitPSH$time)))
title(main = list("Survival Curve with Recurrent Event Data", 
      cex = 0.8, font = 2.3, col = "dark blue"))
mtext("Research Group: AVANCE USE R!", cex = 0.7, font = 2, 
      col = "dark blue", line = 1)
mtext("Software made by: Dr. Carlos Martinez", cex = 0.6, font = 2, 
      col = "dark red", line = 0)
 \donttest{
fitWC<-FitSurvRec(Survrecu(id,time,event)~1,data=MMC.TestSurvRec,
              type="wang-chang")
plot(fitWC$time,fitWC$survfunc,type="s" ,ylim=c(0,1),xlim=c(0,max(fitWC$time)))
  	  title(main = list("Survival Curve with Recurrent Event Data", 
       cex = 0.8, font = 2.3, col = "dark blue"))
        mtext("Research Group: AVANCE USE R!", cex = 0.7, font = 2, 
       col = "dark blue", line = 1)
        mtext("Software made by: Dr. Carlos Martinez", cex = 0.6, font = 2, 
       col = "dark red", line = 0)
              }
}

\keyword{Plot}
\keyword{Survival}
