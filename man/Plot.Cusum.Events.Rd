\name{Plot.Cusum.Events}
\alias{Plot.Cusum.Events}
\title{Plot data with recurrent events}

\description{This function plot data with recurrent events}

\usage{Plot.Cusum.Events(yy, xy = 1, xf= 1, colevent = "blue", colcensor = "red",
                                             ltyx = 1, lwdx = 1)}

\arguments{
  \item{yy}{Data type recurrent events. Examples: TBCplapyr, TBCplathi or TBCpyrthi}
  \item{xy}{Initial unit to start the plotted}
  \item{xf}{Final unit of the plotted}
  \item{colevent}{It is color that identifies the event}
  \item{colcensor}{it is color that identifies the censor}
  \item{ltyx}{The line type. Line types can either be specified as an integer (0\eqn{=}"blank", 1\eqn{=}"solid" (default),2\eqn{=}"dashed",3\eqn{=}"dotted",4\eqn{=}"dotdash", 5\eqn{=}"longdash", 6\eqn{=}"twodash") or as one of the character strings: "blank", "solid", "dashed","dotted", "dotdash"," longdash", or "twodash", where blank uses invisible lines (i.e., does not draw them)}
  \item{lwdx}{The line width, a positive number, defaulting to 1. The interpretation is device-specific, and some devices do not implement line widths less than one. (See the help on the device for details of the interpretation.)}
}

\details{This function print and plot as max 5 units each intent.}

\value{Print the data correspond to the selects units}

\references{\bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de naturaleza recurrente. Tesis doctoral.\emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.}

\author{Dr. \bold{Carlos M. Martinez M.}, <cmmm7031@gmail.com>}

\note{This graph is useful because it facilitates the processes of counting in the units}

\seealso{Plot.Data, Events, Plot. Surv.Rec}

\examples{XL<-data(TBCplapyr)
 #TBCplapyr
 # See, the unit number 1 to 24
 Plot.Cusum.Events(TBCplapyr,1,24,"green","red",2,1)
 # See, the unit number 10 to 12
 Plot.Cusum.Events(TBCplapyr,10,12,"pink","blue",1,3)
 # See, the unit number 5 to 9
 Plot.Cusum.Events(TBCplapyr,5,11,,,2,3)
}

\keyword{plot}
\keyword{Events}
