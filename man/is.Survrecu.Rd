\name{is.Survrecu}
\alias{is.Survrecu}
\title{This function verify if the formula type of survival recurrent is object type newTestSurvRec}

\description{To verify if the create object type Survrecu is a formula model type newTestSurvRec}

\usage{is.Survrecu(x)}

\arguments{
  \item{x}{Object type formula of the class newTestSurvRec}
 }

\value{
 \item{False}{if the object is not type formula}
 \item{True}{if the object is type formula}
 }

\references{\bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de naturaleza recurrente. Tesis doctoral. \emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.// \bold{Pena E., Strawderman R., Hollander, M. (2001)}. Nonparametric Estimation with Recurrent Event Data. \emph{J.A.S.A.} 96, 1299-1315}

\author{Dr. \bold{Carlos M. Martinez M.}, <cmmm7031@gmail.com>}

\seealso{FitSurvRec, Dif.Surv.Rec, Survrecu, FitSurvRec}

\examples{data(MMC.TestSurvRec)
x<-Survrecu(MMC.TestSurvRec$id,MMC.TestSurvRec$time,MMC.TestSurvRec$event)~1
is.Survrecu(x)
}

\keyword{Tests}
\keyword{Survival}
