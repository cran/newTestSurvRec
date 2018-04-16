\name{Dif.Surv.Rec}
\alias{Dif.Surv.Rec}
\title{This function computes statistical difference between two survival curves}

\description{p-values of these tests are computed.}

\usage{Dif.Surv.Rec(XX, type, alfa, beta,gamma,eta)}

\arguments{
\item{XX}{Object type recurrent events data}
\item{type}{"LRrec","Grec","TWrec","PPrec","PMrec","FHrec","CMrec","Mrec","all"}
\item{alfa}{The appropriate choice, see \eqn{w_{z}}. Defect value is equal zero}
\item{beta}{The appropriate choice, see \eqn{w_{z}}. Defect value is equal zero}
\item{gamma}{The appropriate choice, see \eqn{w_{z}}. Defect value is equal zero}
\item{eta}{The appropriate choice, see \eqn{w_{z}}. Defect value is equal zero}
}
\details{This function contains tests to compare survival curves with recurrent events. The curves are estimated using \bold{Pena-Strawderman-Hollander} or  \bold{Wang-Chang} estimator.
\bold{GPLE or PSH model}: \bold{Pena} \emph{et al.} (\bold{2001}) defined an estimator of the survival function to recurrent 
events or \bold{Kaplan-Meier} estimator \bold{GPLE}. They used two counting processes \bold{N} and \bold{Y}. The PSH estimator was defined as,
\deqn{\hat{S}(z) =\prod_{t\leq\,z}\left[1-\frac{\Delta\,N\left(s,z\right)}{Y\left(s,z\right)}\right]}
The authors considered two time scales: one related to calendar time (S) and other related to inter occurrences time (T). So, the counting process \bold{N(s, z)} represents the number of observed events in the calendar period \eqn{[0,s]} with \eqn{t\leq\,z} and \bold{Y(s, z)} represents the number of observed events in the period \eqn{[0,s]} with \eqn{t\geq\,z}. The product-limit estimator was developed by \bold{Pena, Strawderman} and \bold{Hollander}, called PSH. This estimator is useful when the inter occurrence times are assumed to 
represents IID sample from some underlying distribution F. The \bold{GPLE} estimator is defined as: A fundamental assumption of this approach is that individuals have been previously and properly classified in groups according to a stratification variable denote by \emph{r}. Thus, the estimator of the survival curve by each group is defined as,
\deqn{\hat{S}_{r}(z) =\prod_{t\leq\,z}\left[1-\frac{\Delta\,N\left(s,z;r\right)}{Y\left(s,z;r\right)}\right]\!\nabla\:r\:=\!1,2.}
\bold{WC model}: \bold{Wang-Chang} (\bold{1999}) proposed an estimator of the common marginal survivor function in the case where within-unit inter occurrences times are correlated. The correlation structure considered by Wang and Chang (1999) is quite general and contains, the cases particular, both the i.i.d. and multiplicative frailty model as special cases. The WC estimator was defined using two new processes,\eqn{d^{*}} and \eqn{R^{*}}. 
\deqn{\hat{S}(t) =\prod_{T\leq\,t}\left[1-\frac{d^{*}\left(T_{k}\right)}{R^{*}\left(T_{k}\right)}\right]}
The authors try take into account in the definition of \eqn{N} and \eqn{Y} that an individual may have more than one event. In fact, this estimator has the same way as the GPLE estimator but using these two different processes. the index \eqn{d^{*}} represents the sum of the proportion of individuals of the inter occurrences times which are equal to \eqn{t} when there is at least one event. On the other hand,  \eqn{R^{*}} represents an average of the individuals that are at risk time \eqn{t}, where for each individual the average is the number of failures or censored times at least equal to \eqn{t}. This average is done regarding the number of events that there are to each individual and in case \eqn{K} is 0 is divided by 1. For definition more formal see Martinez (2009) and Pena \emph{et. al} (2001). The WC estimator of S eliminates the bias for the product-limit estimator developed by PSH (2001) when the inter occurrences times are correlated within units.However, when applied to i.i.d. inter occurrence times, this estimator is not expected to perform as well as the PSH estimator, especially with regard to efficiency.}

\value{# Dif.Surv.Rec(TBCplapyr,"all",0,0,0,0). Values returned
\tabular{lll}{
 Nomb.Est\tab  Chi.square\tab   p.value\cr
 LRrec   \tab  0.3052411 \tab 0.5806152\cr
 Grec    \tab  1.4448446 \tab 0.2293570\cr
 TWrec   \tab  0.9551746 \tab 0.3284056\cr
 PPrec   \tab  1.1322772 \tab 0.2872901\cr
 PMrec   \tab  1.1430319 \tab 0.2850126\cr
 PPrrec  \tab  1.1834042 \tab 0.2766641\cr
 HFrec   \tab  0.3052411 \tab 0.5806152\cr
 CMrec   \tab  0.3052411 \tab 0.5806152\cr
 Mrec    \tab  1.5298763 \tab 0.2161310\cr
 }
}

\references{\bold{Martinez C., Ramirez, G., Vasquez M.} (\bold{2009}).Pruebas no parametricas para comparar curvas de supervivencia de dos grupos  que experimentan eventos recurrentes. Propuestas. \emph{Revista Ingenieria U.C.},Vol 16, 3, 45-55.//\bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de naturaleza recurrente. Tesis doctoral.\emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.}

\author{Dr. \bold{Carlos M. Martinez M.}, <cmmm7031@gmail.com>}

\seealso{Plot.Event.Rec, Plot.Surv.Rec, Print.Summary}

\examples{data(TBCplapyr)
#Return the p-values of the all tests
Dif.Surv.Rec(TBCplapyr,"all",0,0,0,0)
#Return the p-value of the LRrec test
Dif.Surv.Rec(TBCplapyr)
#Return the p-value of the Grec test
Dif.Surv.Rec(TBCplapyr,"Grec")
#Return the p-values of the CMrec tests
#The CMrec test with this parameters generates LRrec test
Dif.Surv.Rec(TBCplapyr,"all",0,0,0,0)
#The CMrec test with this parameters generates Grec test
Dif.Surv.Rec(TBCplapyr,"all",0,0,1,0)
#The CMrec test with this parameters generates TWrec test
Dif.Surv.Rec(TBCplapyr,"all",0,0,0.5,0)
}

\keyword{Statistical}
\keyword{Tests}