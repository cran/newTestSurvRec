\name{MMC.TestSurvRec}
\alias{MMC.TestSurvRec}
\docType{data}
\title{Migratory Motor Complex}

\description{This contains the Migratoty Motor Complex data}

\usage{data(MMC.TestSurvRec)}

\format{
  A data frame with 99 observations on the following 5 variables.
  \describe{
    \item{\code{j}}{Number of the observation on dataset}
    \item{\code{id}}{ID of each subject. Repeated for each recurrence}
    \item{\code{time}}{recurrence o censoring time}
    \item{\code{event}}{censoring status. All event are 1 for each subject excepting last one that it is 0}
    \item{\code{group}}{A factor with levels \code{Females} \code{Males}}
           } }

\details{The data correspond a study from the Section for Gastroenterology of  Department of Internal Medicine, Ulleal University Hospital of Oslo.%%,0407 Oslo 4; and Section of Medical Statistics, Faculty of Medicine, University of Oslo, 0316 Oslo 3, Norway". This study was supported by grants from "the Norwegian Medical Research Council and Helga Sembs Foundation."
} 

\source{\bold{Husebye E, Skar V, Aalen O. and Osnes M} (\bold{1990}), Digestive Diseases and Sciences.}

\references{\bold{Husebye E, Skar V, Aalen O.O., Osnes M.}(\bold{1990}). Digital ambulatory manometry of the small intestine in healthy adults. Estimates of variation within and between individuals and statistical management of incomplete MMC periods. Digestive Diseases and Sciences.35:1057: 65.}

\examples{
data(MMC.TestSurvRec)
XL<-data(MMC.TestSurvRec)
print(XL)
Print.Summary(MMC.TestSurvRec)
## maybe str(MMC.TestSurvRec) ; plot(MMC.TestSurvRec) ...
}

\keyword{datasets}
