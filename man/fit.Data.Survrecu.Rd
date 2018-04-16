\name{fit.Data.Survrecu}
\alias{fit.Data.Survrecu}

\title{This function let to adjust the IDs the database}

\description{This function let to adjust the ID`s the database in case that it is not have the order  numeric correct. Observation: this function only let to adjust the id variable not sort the rest of the data.}

\usage{fit.Data.Survrecu(x)}

\arguments{\item{x}{a database type dataframe}}

\value{Returns the correct numeric order for the dataframe}

\references{\bold{Martinez C., Ramirez, G., Vasquez M.} (\bold{2009}).Pruebas no parametricas para comparar curvas de supervivencia de dos grupos que experimentan eventos recurrentes. Propuestas. \emph{Revista Ingenieria U.C.},Vol 16, 3, 45-55.//\bold{Pena E., Strawderman R., Hollander M. (2001)}. Nonparametric Estimation with Recurrent  Event Data. \emph{J.A.S.A.} 96, 1299-1315}

\author{Dr. \bold{Carlos M. Martinez M.}, <cmmm7031@gmail.com>}

\note{The last id on each unit of the database to have be a censored data and the occurrences  have that to precede to this last it.}

\seealso{FitSurvRec, Survrecu, is.Survrecu}

\examples{data(MMC.TestSurvRec)
ID<-fit.Data.Survrecu(Survrecu(MMC.TestSurvRec$id,MMC.TestSurvRec$time,
                               MMC.TestSurvRec$event))
ID
fit<-PSH.fit(Survrecu(ID,MMC.TestSurvRec$time,
                               MMC.TestSurvRec$event))
fit$time
fit$surv
plot(fit$time,fit$surv)

 \donttest{
data(DataColonDukesABvsD)
XL<-data(DataColonDukesABvsD)
DataColonDukesABvsD$Iden
Y<-fit.Data.Survrecu(Survrecu(DataColonDukesABvsD$Iden,DataColonDukesABvsD$time,
                                  DataColonDukesABvsD$event))
Y
fit<-WC.fit(Survrecu(Y,DataColonDukesABvsD$time,DataColonDukesABvsD$event))
fit$time
fit$surv
plot(fit$time,fit$surv)
print(data.frame(time=fit$time,n.event=fit$n.event, 
                     Surv=fit$survfunc,std.error=fit$std.error))
}

}


\keyword{dataframe}
\keyword{data}