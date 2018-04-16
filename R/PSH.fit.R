PSH.fit <-
function (x, tvals) {
    #if (!is.Survrecu(x)) {
    #    stop("\n x must be a Survrecu object")
    #}
    x[, 1]<-fit.Data.Survrecu(x)
    n <- length(unique(x[, 1]))
    failed <- c(x[, 2][x[, 3] == 1])
    censored <- c(x[, 2][x[, 3] == 0])
    m <- table(x[, 1]) - 1
    sfailed <- sort(failed)
    nfailed <- length(failed)
    xsumm4<-as.double(sfailed)
 xsumm5<-as.integer(nfailed)
xsumm6<-as.double(censored)
 xsumm7<-as.integer(length(unique(xsumm4)))
 xsumm8<-unique(sort(as.double(failed)))
 xsumm9<-as.integer(rep(0, nfailed))
xsumm10<-as.integer(rep(0,n * nfailed))
    #---------------------------------------------------------------
    numdistinct <-  xsumm7  
    distinct <-xsumm8 
    numdeaths <-as.double(table(as.double(sfailed)))
    #--------------------------------------------------------------- 
    #--------------------------------------------------------------- 
    #     Acumula tiempos de ocurrencias de eventos en unidades 
    #--------------------------------------------------------------- 
    y4<-matrix(0, nrow = (length(x)/3), ncol = 7)
    y4[,1:3]<-x 
    y4[,6]<-0     
    yacum<-y4[1,2] 
    for (i in 1:(length(x)/3-1)){
    for (j in 2:(length(x)/3)){ 
       if (y4[j,1]==y4[j-1,1]) {yacum<-yacum+y4[j,2];
                                y4[j,4]<-yacum;
                                yacum<-y4[j,4]} 
                                else {yacum<-y4[j,2];y4[j,4]<-y4[j,2]} 
                              }}
    y4[1,4]<-y4[1,2]
    y4[,5]<-y4[,3]*y4[,4]
    for (i in 1:(length(x)/3-1)){
      if (y4[i,3]==0) {y4[i,7]<-y4[i,4];y4[i,6]<-1}
      } 
    #---------------------------------------------------------------
    vAtR<-matrix(0, nrow = n, ncol = numdistinct)
    vAuxTiempo<-matrix(0, nrow = n, ncol = numdistinct)
    vAtR[,1]<-table(x[, 1])
    yt<-distinct
    
    vAuxO<-matrix(0, nrow = n, ncol = numdistinct)
    for (i  in 1:nrow(y4)){
    acum<-0   
    for (j in 1:length(as.integer(distinct))){
            a<-y4[i,1]
         if (yt[j]<=y4[i,2] && y4[i,3]==1) {acum<-1;vAuxO[a,j]<-vAuxO[a,j]+1}
                                             }}
    vAux1<-matrix(0, nrow = n, ncol = numdistinct)
    for (i  in 1:nrow(y4)){
    for (j in 1:length(as.integer(distinct))){
            a<-y4[i,1]
         if (yt[j]<=y4[i,2] && y4[i,3]==0) vAux1[a,j]<-1
                                            }}
    vAux2<-matrix(0, nrow = n, ncol = numdistinct)
    for (i  in 1:nrow(y4)){
    for (j in 1:1){
            a<-y4[i,1]
         if (yt[1]>y4[i,2] && y4[i,3]==0) vAux2[a,j]<-1
                                             }}
    
    vAcumR<-vAuxO+vAux1
    vAcumR[,1]<-table(x[, 1])-vAux2[,1]
       
    #---------------------------------------------------------------
    vAtRisk <-vAcumR
    AtRisk <- matrix(vAtRisk, nrow = n, ncol = numdistinct)
    survfuncPSHple <- vector("numeric", numdistinct)
    AtRiskTotals <- t(AtRisk) %*% c(rep(1, n))
    survfuncPSHple.i <- 1 - (numdeaths/AtRiskTotals)
    survfuncPSHple <- cumprod(survfuncPSHple.i)
    se.NA <- cumsum(numdeaths/(AtRiskTotals)^2)
    se.PLE <- sqrt(se.NA * survfuncPSHple^2)
    #---------------------------------------------------------------
    if (!missing(tvals)) {tvalslen <- length(tvals);
        tvals.o <- sort(tvals);
        #PSHpleAttvals <- surv.search(tvals.o, distinct, survfuncPSHple)
        } else {tvals <- NA}#;PSHpleAttvals <- NA}
    #---------------------------------------------------------------
    ans <-list(n = n, m = m, failed = failed, censored = censored, 
        time = distinct, n.event = numdeaths, AtRisk = AtRisk, 
        survfunc = survfuncPSHple, std.error = se.PLE, tvals = tvals)
    oldClass(ans) <- "FitSurvRec"
    return(ans) }
