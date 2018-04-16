\name{newTestSurvRec-package}
\alias{newTestSurvRec-package}
\alias{newTestSurvRec}
\docType{package}

\title{package newTestSurvRec}

\description{package newTestSurvRec}

\details{Recurrent events are common in many areas: \emph{psychology, engineering, medicine, physics, astronomy, biology, economics} and \emph{so on}. Such events are very common in the real world: \emph{viral diseases, carcinogenic tumors, machinery and equipment failures, births, murders, rain, industrial accidents, car accidents}  and \emph{so on}. The availability of computerized tools for the analysis is indispensable. The survival analysis is a branch of statistics that allows us to model the time until the occurrence of an events. In general, the objectives of analysis are: the modeling of the survival function to estimate the risk or benefit of the occurrence of an event, the probability occurrence of this event and comparing population groups. The development of tools for the statistical analysis of recurrent event is relatively recent and are not fully known. The purpose of this package is to present statistical tests for the analysis of recurrent event data. \bold{Martinez} \emph{et al. }(\bold{2009}) published a statistical test to compare survival curves of two groups with recurrent events. The hypothesis of the problem is:
					\deqn{\bold{H_{o} : S_{1}(t) = S_{2}(t)}} 
					\deqn{\bold{H_{1} : S_{1}(t) \neq S_{2}(t)}}

Where, \eqn{S_{1}(t)} and \eqn{S_{2}(t)} are the survival curves of the both group. The statistic of test is,
\deqn{Z=\frac{\sum^{\,}_{t\leq\,z}w_{z}\left[\Delta\,N(s,z;r)-E\left\{\Delta\,N(s,z;r)\right\} \right]}{\sqrt{\sum^{\,}_{t\leq\,z}w^{2}_{z}Var\left\{\Delta\,N(s,z;r) \right\}}}}
The statistic Z has a normal asymptotic behavior. Its square has a chi-square approximate behavior with a degree of freedom.
So,\deqn{\Delta\,N(s, z; 1) = N(s, z+\Delta\,z; 1) - N(s, z; 1)}
Now, \eqn{\Delta\,z} is approaches to zero and as \eqn{\Delta\,N(s, z; 1)} has a hyper-geometric behavior and expected value is equal to 
\deqn{Y(s, z; 1)\Delta\,N(s, z)/Y(s, z)} 
and variance equal to,
\deqn{Var\left[\Delta\,N(s, z; 1) \right]=\frac{Y(s,z)-Y(s,z;1)}{Y(s,z)-1}Y(s,z;1)\frac{\Delta\,N(s,z)}{Y(s,z)}\left[1-\frac{\Delta\,N(s,z)}{Y(s,z)}\right]}
This author proposed various types of weights \eqn{(w_{z})},
\deqn{w_{z}=\left[S(z)\right]^{\gamma}\left[1-S(z)\right]^{\eta}\frac{\left[Y(s,z)\right]^{\alpha}}{\left[Y(s,z)+1\right]^{\beta}}}
The appropriate selection of weights depends on the behavior of the curves. With the selection of the values of the parameters \eqn{(\alpha,\;\beta,\;\gamma\;and \;\eta)}, on the proposal, is possible  adjust its behavior. With the proposal, we are able of make studies on survival analysis with recurrent events and generate tests for analysis others, including the classical tests type: \bold{logrank, Gehan, Peto-Peto, Fleming-Harrington} and so on. Note that, if all parameters are zero, \eqn{w_{z} = 1}, its generates the test type logrank for recurrent events. If, \eqn{\alpha = 1} and the other parameters are zero \eqn{w_{z} = Y(s,z)}, its generates the test type \bold{Gehan}. If, \eqn{\gamma=1} and the other parameters are zero \eqn{w_{z} = S(z)}, its generate the test of \bold{Peto-Peto}. If, \eqn{\gamma = 1} and \eqn{\eta = 1} and the rest of the parameters are zero, its generate \bold{Fleming-Harrington} test. On the other hand, if you analyze the test statistical of comparison for recurrent events, it depends on the counting processes \bold{N} and \bold{Y}, which are doubles indexed. The index S measures calendar time and Z index measures the gap times. So, if the observation time tends to infinity and unity event study can only occur once in each unit and the statistical comparison becomes the weighted classical statistical comparison of groups of the survival analysis. We can conclude that test proposed by \bold{Martinez} \emph{et al.}(\bold{2009}) are useful on diverse fields of research, such as: \emph{medicine, public health, insurance, social science, reliability} and \emph{ others.}
}

\author{Dr. \bold{Carlos M. Martinez M.}, <cmmm7031@gmail.com>}

\note{This package have some functions that them were originally performed by the survrec package, which solved the adjustment problem of the PSH and WC estimators using Fortran routines. With the permission of the author, Dr. Juan R. Gonzalez, the algorithm of base was taken, modified, the algorithm, WC estimator was reprogrammed and adapted to the needs of the newTestSurvRec package and thus avoid dependence. Thanks to Dr. Gonzalez}

\references{\bold{Martinez C., Ramirez, G., Vasquez M.} (\bold{2009}).Pruebas no parametricas para comparar curvas de supervivencia de dos grupos  que experimentan eventos recurrentes. Propuestas. \emph{Revista Ingenieria U.C.},Vol 16, 3, 45-55.// \bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de  naturaleza recurrente. Tesis doctoral.\emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.// \bold{Pena E., Strawderman R., Hollander, M. (2001)}. Nonparametric Estimation with Recurrent  Event Data. \emph{J.A.S.A.} 96, 1299-1315.}

\seealso{Dif.Surv.Rec, Plot.Event, Rec,Plot.Surv. Rec, Print.Summary, Plot.Cusum.Events}

\examples{##library(newTestSurvRec)
getOption("defaultPackages")
XL<-data(TBCplapyr)
XL
Plot.Event.Rec(TBCplapyr)
Dif.Surv.Rec(TBCplapyr,'all',1,1,0,0)
Print.Summary(TBCplapyr)
}

\keyword{package}

