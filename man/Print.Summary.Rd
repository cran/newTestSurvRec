\name{Print.Summary}
\alias{Print.Summary}

\title{Function to print summary of statistics tests to comparison of the survival curves of the groups with recurrent events}

\description{Returns matrices that contain the estimations of the survival curves for both groups. The estimations of survival curves of both groups are made using PSH estimator. The p.values of the tests are returned.}

\usage{Print.Summary(XX,...)}

\arguments{
  \item{XX}{Object type recurrent events data}
  \item{...}{other objects}
}
\details{See Dif.Surv.Rec(XX,...)
}
\value{Put object type recurrent events data. 
#Print.Summary(TBCplapyr). #Values returned:

\tabular{lllll}{
 time \tab  n.event\tab  n.risk\tab  Surv_G1 \tab std.error\cr
    1 \tab        2\tab     127\tab  0.984\tab    0.0110\cr
    2 \tab        9\tab     124\tab  0.913\tab    0.0243\cr
    3 \tab       14\tab     113\tab  0.800\tab    0.0340\cr
    4 \tab        9\tab      98\tab  0.726\tab    0.0380\cr
   ...\tab       ..\tab      ..\tab  .....\tab    ......\cr
   ...\tab       ..\tab      ..\tab  .....\tab    ......\cr   
   29 \tab        1\tab      18\tab  0.244\tab    0.0422\cr
   31 \tab        1\tab      13\tab  0.225\tab    0.0427\cr
   35 \tab        1\tab       9\tab  0.200\tab    0.0439\cr
}
      
\tabular{lllll}{
  time\tab n.event\tab n.risk\tab Surv_G2 \tab std.error\cr
     1\tab       3\tab     84\tab 0.964\tab    0.0199\cr
     2\tab       6\tab     81\tab 0.893\tab    0.0327\cr
     3\tab      12\tab     73\tab 0.746\tab    0.0447\cr
     4\tab      10\tab     61\tab 0.624\tab    0.0494\cr
   ...\tab      ..\tab     ..\tab .....\tab    ......\cr
   ...\tab      ..\tab     ..\tab .....\tab    ......\cr  
    15\tab       1\tab     17\tab 0.283\tab    0.0514\cr
    42\tab       1\tab      6\tab 0.236\tab    0.0582\cr
    44\tab       1\tab      5\tab 0.189\tab    0.0599\cr
}
          Group Median
\tabular{ll}{
        Group\tab Median\cr
 Pooled Group\tab      8\cr
    1er Group\tab      9\cr
    2do Group\tab      6\cr
}
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
\references{\bold{Martinez, C. (2009)}. Generalizacion de algunas pruebas clasicas de comparacion de curvas de supervivencia al caso de eventos de naturaleza recurrente. Tesis doctoral.\emph{ Universidad Central de Venezuela (UCV)}. Caracas-Venezuela.}

\author{Dr. \bold{Carlos M. Martinez M.} <cmmm7031@gmail.com>}

\seealso{Dif.Surv.Rec, Plot.Surv.Rec}

\examples{data(TBCplapyr)
Print.Summary(TBCplapyr)
}

\keyword{Plot}
\keyword{Survival}


