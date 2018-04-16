\name{Plot.Event.Rec}
\alias{Plot.Event.Rec}

\title{This function plots the ocurrence of a event in two scales time}

\description{Recurrent events are plotted. A plot is returned. The counting processes are a powerful tools in survival analysis. These process consider two scale time, a calendar time and a gap time. This idea originally provides from \bold{Gill } (\bold{1981}) and the concept was extended by \bold{Pena} \emph{et al. } (\bold{2001}).}

\usage{Plot.Event.Rec(yy, xy, xf)}

\arguments{
  \item{yy}{Object type recurrent events data. Example: TBCplapyr}
  \item{xy}{Identification of the unit to plotted. 'xy = 1' is defect value.}
  \item{xf}{Argument to plot the ocurrent events of the unit 'xf'. 'xf = 1' is defect value.}
}
\value{Plot is returned.\bold{ Pena } \emph{et al.} (\bold{2001}) designed a special graphic, that allows to count the occurrence of events per unit time. Doubly-indexed processes illustration for an case. The graphic shows a case followed during 24.01 months. This patient presents four recurrences at months 7, 10, 16 and 24 from the beginning of study. This fact implies that interoccurrence. times are 7, 3, 6, 8 and the censored time correspond to 0.01 months. Let us assume that we are interested in computing the single processes, N(t) and Y (t) for a selected interoccurrence time t = 5. In this case N(t = 5) = 1 and Y (t = 5) = 3. For the calendar time scale, s = 20, we have N(s = 20) = 3 and Y (s = 20) = 1. Now, let us assume that we would like to know double-indexed processes for both selected interoccurrence and calendar times. Using both time scales we observe that \eqn{N_{14}(s = 20,t = 5)=1}, \eqn{Y_{14}(s = 20, t = 5) = 2 } and \eqn{\Delta\,N_{14}(s = 20,t = 6) = 1}.
}
\references{\bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de naturaleza recurrente. Tesis doctoral.\emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.// \bold{Pena E., Strawderman R., Hollander, M. } (\bold{2001}). Nonparametric Estimation with Recurrent Event Data. \emph{ J.A.S.A.} 96, 1299-1315.// \bold{Gill, R. }(\bold{1981}) Testing with replacement and the product-limit estimator. \emph{ Ann. Statist.}, 9, 853-860.
}

\author{Dr. \bold{ Carlos M. Martinez M.} <cmmm7031@gmail.com>}

\seealso{Dif.Surv.Rec, Plot.Data.Events}

\examples{XL<-data(TBCplapyr)
# See, the unit number 14
Plot.Event.Rec(TBCplapyr,14,14)
# See, the unit number 5 
Plot.Event.Rec(TBCplapyr,5,5)
}

\keyword{Event}
\keyword{Plot}
