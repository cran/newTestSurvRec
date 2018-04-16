\name{Survrecu}
\alias{Survrecu}

\title{Create a Survival recurrent object type newTestSurvRec}

\description{Create a survival recurrent object, usually used as a response variable in a model formula}

\usage{Survrecu(id, time, event)}

\arguments{
  \item{id}{Identifier of each subject. This value is the same for all recurrent times of each subject.}
  \item{time}{time of recurrence. For each subject the last time are censored.}
  \item{event}{The status indicator, 0=no recurrence 1=recurrence. Only these values are accepted.}
}

\value{An object of class newTestSurvRec is returned. newTestSurRec object is implemented as a matrix of 3 colummns. No method for print. In the case of is.Survrecu, a logical value TRUE if x inherits from class Survrecu, otherwise an FALSE.}

\references{\bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de naturaleza recurrente. Tesis doctoral.\emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.}

\author{Dr. \bold{Carlos M. Martinez M.}, <cmmm7031@gmail.com>}

\seealso{FitSurvRec, is.Survrecu}

\examples{data(MMC.TestSurvRec)
Survrecu(MMC.TestSurvRec$id,MMC.TestSurvRec$time,MMC.TestSurvRec$event)~1}

\keyword{Tests}
\keyword{Survival}
