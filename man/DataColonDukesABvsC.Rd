\name{DataColonDukesABvsC}
\alias{DataColonDukesABvsC}
\docType{data}
\title{Re-hospitalization of patients with colorectal cancer}

\description{This data set contains the re-hospitalization times of patients diagnosed with stage AB and  patients diagnosed with stage C.}

\usage{data(DataColonDukesABvsC)}

\format{ A data frame with 655 observations on the following 10 variables.
\describe{This data.frame contains the following columns
    \item{\code{j}}{Observation number}
    \item{\code{Iden}}{identification of each subject. Repeated for each recurrence}
    \item{\code{id}}{identification of each subject. Repeated for each recurrence}
    \item{\code{Tinicio}}{Initial time of observation just before each recurrence}
    \item{\code{time}}{re-hospitalization o censoring gaptime}
    \item{\code{Tcal}}{re-hospitalization o censoring calendar time}
    \item{\code{event}}{censoring status. All event are 1 for each subject excepting last one that it is 0}
    \item{\code{chemoter}}{Did patient receive chemotherapy? 1: No or 2:Yes}
    \item{\code{dukes}}{Dukes tumor stage: 1:A-B or 2:C}
    \item{\code{distance}}{distance from living place to hospital 1:<=30 Km. or  2:>30 Km.}
}
}

\details{The patients included in the study have been operated between January 1996 and December 1998. For each patient, we have considered this date as the beginning of the observational period. All patients were followed until June 2002. Consequently, the length of the monitoring period can differ for each patient, depending on its surgery date. The first inter occurrence time has been considered as the time between the surgical intervention and the first hospitalization related to cancer. Four hundred and three patients with colon and rectum cancer have been included in the study. Information about their sex (male or female), age ( 60, 60-74 or  75), and tumor stage using Dukes classification (A-B, C, or D) have been recorded. The following inter- occurrence times have been considered as the difference between the last hospitalization and the current one. Only re-admissions related to cancer have been considered.}

\source{This data were obtained from \bold{Gonzalez, J.R.} \emph{et al.} (\bold{2009})}

\references{\bold{Martinez C., Ramirez, G., Vasquez M.} (\bold{2009}). Pruebas no parametricas para comparar curvas de supervivencia de dos grupos que experimentan eventos recurrentes. Propuestas. \emph{Revista Ingenieria U.C.},Vol 16, 3, 45-55.// \bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de  naturaleza recurrente. Tesis doctoral.\emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.//\bold{Gonzalez, J.R., Fernandez, E., Moreno, V.} \emph{et al.} Gender differences in hospital readmission among colorectal cancer patients. Currently submited to J.C.O.}

\examples{data(DataColonDukesABvsC)}

\keyword{datasets}
