\name{DataColonDukesABvsD}
\alias{DataColonDukesABvsD}
\docType{data}
\title{Re-hospitalization of patients with colorectal cancer}
\description{This data contains re-hospitalization times of patients diagnosed with stage AB and patients diagnosed with stage D.}

\usage{data(DataColonDukesABvsD)}
\format{
 A data frame with 527 observations on the following 10 variables
  \describe{This data.frame contains the following columns:
    \item{\code{j}}{Observation number}
    \item{\code{Iden}}{Observation of each subject. Repeated for each recurrence}
    \item{\code{id}}{Observation of each subject. Repeated for each recurrence}
    \item{\code{Tinicio}}{Initial time of observation just before each recurrence}
    \item{\code{time}}{re-hospitalization o censoring gaptime}
    \item{\code{Tcal}}{re-hospitalization o censoring calendar time}
    \item{\code{event}}{censoring status. All event are 1 for each subject excepting last one that it is 0}
    \item{\code{chemoter}}{Did patient receive chemotherapy? 1: No or 2:Yes}
    \item{\code{dukes}}{Dukes tumoral stage: 1:A-B or 3:D}
    \item{\code{distance}}{distance from living place to hospital 1:<=30 Km. or 2:>30 Km.}
  }
}

\details{See details on DataColonDukesABvs}

\source{This data were obtained from \bold{Gonzalez, J. R.} \emph{et al.} (\bold{2009})}

\references{\bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de naturaleza recurrente. Tesis doctoral.\emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.// \bold{Gonzalez, J.R., Fernandez, E., Moreno, V. } \emph{et al.} Gender differences in hospital readmission among colorectal cancer patients. Currently submited to J.C.O.}

\examples{data(DataColonDukesABvsD)
XL<-data(DataColonDukesABvsD)
print(XL)
}

\keyword{datasets}
