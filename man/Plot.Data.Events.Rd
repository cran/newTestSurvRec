\name{Plot.Data.Events}
\alias{Plot.Data.Events}
\title{Plot data with recurrent events}

\description{This function plot data with recurrent events}

\usage{Plot.Data.Events(yy, paciente, inicio, dias, censored, especiales,
                                          colevent="red",colcensor="blue")}

\arguments{
\item{yy}{Data type recurrent events. Examples: TBCplapyr, TBCplathi or TBCpyrthi}
\item{paciente}{Vector of number of units on the data base}
\item{inicio}{Vector, its assumed that the units are observed from one time equal to zero.}
\item{dias}{Vector of the periods of observations of the study untis}
\item{censored}{vector of times of censorship for each unit}
\item{especiales}{Three-column matrix containing the identification of the units of study in each observation, the times of occurrence of the event or censorship and type of event.}
\item{colevent}{Color event identifier.}
\item{colcensor}{ Color censored data identifier.}
}

\details{The plot shows the recuurence of the events on the time}

\value{This function returned the pictorial representation of the set of recurrence events data}

\references{\bold{Martinez C., Ramirez, G., Vasquez M.} (\bold{2009}).Pruebas no parametricas para comparar curvas de supervivencia de dos grupos que experimentan eventos recurrentes. Propuestas. \emph{Revista Ingenieria U.C.},Vol 16, 3, 45-55.// \bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de naturaleza recurrente. Tesis doctoral.\emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.}

\author{Dr. \bold{Carlos M. Martinez M.}, <cmmm7031@gmail.com>}

\note{We recommend users to use routines similar to the example.}

\seealso{Dif.Surv.Rec, Plot.Surv.Rec, Print.Summary}

\examples{data(TBCplapyr)
XL<-data(TBCplapyr)
p<-ncol(TBCplapyr)
N<-nrow(TBCplapyr)
censor<-matrix(TBCplapyr$event)
especiales<-matrix(data=0,nrow(TBCplapyr),3)
especiales[,1]<-matrix(TBCplapyr$id)
especiales[,2]<-matrix(TBCplapyr$Tcal)
especiales[,3]<-matrix(TBCplapyr$event)
niveles<-levels(factor(especiales[,1]))
for(i in 1:N){
for(j in 1:nrow(matrix(niveles))){
if (as.character(especiales[i,1])==niveles[j]) especiales[i,1]<-j}}
StudyPeriod<-matrix(data=0,nrow(matrix(niveles)),1)
start<-matrix(data=0,nrow(matrix(niveles)),1)
k<-0
for(j in 1:N){if (TBCplapyr$event[j]==0){k<-k+1;StudyPeriod[k,1]<-TBCplapyr$Tcal[j]}}
units<-matrix(1:nrow(matrix(niveles)),nrow(matrix(niveles)),1)
Plot.Data.Events(TBCplapyr,units,start,StudyPeriod,censor,especiales,"black","blue")
Plot.Data.Events(TBCplapyr,units,start,StudyPeriod,censor,especiales,"red","black")
}
\keyword{Recurrence}
\keyword{Events}