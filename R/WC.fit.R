WC.fit <-
function (x, tvals){
    #if (!is.Survrecu(x)) {
    #    stop("\n x must be a Survrecu object")
    #}
    x[, 1]<-fit.Data.Survrecu(x)
    n <- length(unique(x[, 1]))
    failed <- c(x[, 2][x[, 3] == 1])
      sfailed <- sort(failed)
      nfailed <- length(failed)  
    gap <- c(x[, 2])
    cen.gap <- c(x[, 2][x[, 3] == 0])
    event <- c(x[, 3])
    tot <- length(gap)
     distinct <- sort(unique(c(x[, 2][x[, 3] == 1])))
     ndistinct <- length(distinct)
     uid <- unique(x[, 1])
     m <- as.integer(table(x[, 1]))
     mMax <- max(m)
    wt <- rep(1, n)
    tiempos<-sort(unique(x[,2]))
    Nocurrencia<-table(x[,2][x[,3]==1])
    Ki<-c(table(x[,1][x[,3]==1]))
    Ik1<-m
    Ik2<-m-1
    for (i in length(m)){if (Ik2[i]>1) Ik2[i]<-1}
      b<-table(x[,1][x[,3]==1]);
      x1<-data.frame(id=x[,1],time=x[,2],event=x[,3],toc=x[,2]*x[,3])
      f<-table(x1[,1])-1
    #--------------------------------------------------------------------
    #--------------------------------------------------------------------
    #                   Calcula d*(t)
    #--------------------------------------------------------------------    
      dt<-c()
      a<-1
      for (p in 1:length(distinct)){
           acum<-0;ditp<-0;
         tki <- table(x1[,1][x1[,3]==1]);
            for (i in 1:length(x[,1])){
             if (x1[i,2]==distinct[p]&& x1[i,3]==1) ki<-m[x1[i,1]]-1;
             ditp<- sum(table(x1[i,3][x1[i,4]*x1[i,3] == distinct[p]]))
             if (x1[i,2]==distinct[p]&& x1[i,3]==1 && ki>0) {s<-x1[i,3]*ditp/ki;acum<-acum+s}
                                  }
      dt<-append(dt,acum)          }
    #--------------------------------------------------------------------
    #                   Calcula R*(t)            
    #--------------------------------------------------------------------
      x1<-data.frame(id=x[,1],time=x[,2],event=x[,3],toc=x[,2]*x[,3],
                        r=0*x[,2]+1,toc1=x[,3])
      for (s in 1:length(x1[,1])){if (x1$toc1[s]==2) {x1$toc1[s]<-0}}
      Neventos<-table(x1[,1])
      KiAst<-0*(x1[,1])
      for (p in 1:length(x1[,1])){if (Neventos[x1[p,1]]<=1) {KiAst[p]<-1} else {KiAst[p]<-Neventos[x1[p,1]]-1}}
           
      x1<-data.frame(id=x[,1],time=x[,2],event=x[,3],toc=x[,2]*x[,3],
                        r=0*x[,2]+1,toc1=x[,3],roc2=KiAst)
 
      Neventos<-table(x1[,1])
      for (p in 1:length(Neventos)){if (Neventos[p]<=1) {Neventos[p]<-1} else {Neventos[p]<-Neventos[p]-1}}
        Rt<-c()
        for (p in 1:length(distinct)){
                r1<-0;r2<-0;
                for (i in 1:length(x1[,1])){
                     n<-x1[i,1]
                     if (x1[i,2]>=distinct[p] && x1[i,3]==1) {r1<-r1+1/as.integer(Neventos[n])};
                     if (x1[i,2]>=distinct[p] && x1[i,3]==0 && m[n]<2) {r2<-r2+1/as.integer(Neventos[n])}
                                           }
        Rt<-append(Rt,r1+r2)         } 
#--------------------------------------------------------------------
#         Estimator of Survival function
#--------------------------------------------------------------------
           pi<-1-dt/Rt
           Survival<-cumprod(pi)
           survfuncWCple <- round(Survival,8)
      #--------------------------------------------------------------------
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
   vAtR<-matrix(0, nrow = n, ncol = ndistinct)
    vAuxTiempo<-matrix(0, nrow = n, ncol = ndistinct)
    vAtR[,1]<-table(x[, 1])
    yt<-distinct
    
    vAuxO<-matrix(0, nrow = n, ncol = ndistinct)
    for (i  in 1:nrow(y4)){
    acum<-0   
    for (j in 1:length(as.integer(distinct))){
            a<-y4[i,1]
         if (yt[j]<=y4[i,2] && y4[i,3]==1) {acum<-1;vAuxO[a,j]<-vAuxO[a,j]+1}
                                             }}
    vAux1<-matrix(0, nrow = n, ncol = ndistinct)
    for (i  in 1:nrow(y4)){
    for (j in 1:length(as.integer(distinct))){
            a<-y4[i,1]
         if (yt[j]<=y4[i,2] && y4[i,3]==0) vAux1[a,j]<-1
                                            }}
    vAux2<-matrix(0, nrow = n, ncol = ndistinct)
    for (i  in 1:nrow(y4)){
    for (j in 1:1){
            a<-y4[i,1]
         if (yt[1]>y4[i,2] && y4[i,3]==0) vAux2[a,j]<-1
                                             }}
    
    vAcumR<-vAuxO+vAux1
    vAcumR[,1]<-table(x[, 1])-vAux2[,1]
#----------------------------------------
      vAtRisk <-vAcumR
    AtRisk <- matrix(vAtRisk, nrow = n, ncol = ndistinct)
      AtRiskTotals <- t(AtRisk) %*% c(rep(1, n))
    #---------------------------------------------------------------
    se.NA <- cumsum(numdeaths/(AtRiskTotals)^2)
    se.WC <- sqrt(se.NA * survfuncWCple^2)
    #---------------------------------------------------------------
#--------------------------------------------------------------------  
   if (!missing(tvals)) {tvalslen <- length(tvals)
                            tvals.o <- sort(tvals)}
    else {
        tvals <- NA
        WCpleAttvals <- NA}
    ans <- list(n = n, m = m, failed = failed, censored = cen.gap, 
        time = distinct, n.event = dt, AtRisk = Rt, survfunc = survfuncWCple, 
        std.error = se.WC, tvals = tvals)
    oldClass(ans) <- "FitSurvRec"
    return(ans)}
