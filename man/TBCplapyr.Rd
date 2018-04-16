\name{TBCplapyr}
\alias{TBCplapyr}
\docType{data}
\title{Data  in patients with bladder cancer treated with placebo or pyridoxine}
\description{This database corresponds to the time of recurrence of tumors in 78 patients with bladder cancer. Patients were randomly assigned to treatments: placebo (47 patients) and pyridoxine (31 patients). Data type data.frame with 222 observations on 8 variables.}
\usage{data(TBCplapyr)}

\format{ A data frame with 222 observations on the following 9 variables.
  \describe{
    \item{\code{j}}{Observation number}
    \item{\code{id}}{ID of each unit. Repeated for each recurrence}
    \item{\code{Tinicio}}{Inicial time}
    \item{\code{time}}{recurrence o censoring time. For each unit the last time is censored}
    \item{\code{Tcal}}{Time if observation for each unit}
    \item{\code{event}}{censoring status. 1 = occurrence of the event in the unit and 0 right censored time}
    \item{\code{strata}}{Number of strata}
    \item{\code{trt}}{a factor with levels \eqn{Placebo} or \eqn{Pyridoxine}}
    \item{\code{group}}{A factor with levels. Group identification}
  }
}

\details{Experiment \bold{Byar}(\bold{1980}). The database Byar experiment is used and the time (months) of recurrence of tumors in 116 sick patients with superficial bladder cancer is measured. These patients were randomly allocated to the following treatments: placebo (47 patients), pyridoxine (31 patients) and thiotepa (38 patients). }

\source{\bold{Andrews D. }, \bold{Herzberg A.}, (\bold{1985}). Data. A collections of problems from many fields for the student and reserarch worker, Springer series in statistics, Springer-Verlag, USA}

\references{\bold{Martinez C., Ramirez, G., Vasquez M.} (\bold{2009}).Pruebas no parametricas para comparar curvas de supervivencia de dos grupos que experimentan eventos recurrentes. Propuestas. \emph{Revista Ingenieria U.C.},Vol 16, 3, 45-55.// \bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de naturaleza recurrente. Tesis doctoral.\emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.// \bold{Pena E., Strawderman R., Hollander M.} (\bold{2001}). Nonparametric Estimation with Recurrent Event Data. \emph{J.A.S.A.} 96, 1299-1315.}

\examples{XL<-data(TBCplapyr)
XL<-data(TBCplapyr)
print(XL)
Print.Summary(TBCplapyr)
}
\keyword{datasets}
