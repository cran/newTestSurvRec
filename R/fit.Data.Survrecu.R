fit.Data.Survrecu <-
function (x) {
    n <- length(unique(x[, 1]))
    failed <- c(x[, 2][x[, 3] == 1])
    censored <- c(x[, 2][x[, 3] == 0])
    m <- table(x[, 1]) - 1
    sfailed <- sort(failed)
    nfailed <- length(failed)
    y4<-matrix(0, nrow = (length(x)/3), ncol = 7)
    y4[,1:3]<-x 
    y4[1,6]<-1     
    yacum<-y4[1,6]
    for (i in 1:(length(x)/3-1)){
    for (j in 2:(length(x)/3)){ 
       if (y4[j,1]==y4[j-1,1]) {yacum<-y4[j-1,6];
                                y4[j,6]<-yacum;
                                yacum<-y4[j,6]} 
                                else {yacum<-y4[j-1,6]+1;y4[j,6]<-yacum}
                            }}
     x[,1]<-y4[,6]
     return(x[,1])         }
