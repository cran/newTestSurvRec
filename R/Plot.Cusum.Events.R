Plot.Cusum.Events <-
function (yy, xy = 1, xf = 1,colevent="blue",
                             colcensor="red",ltyx = 1,lwdx=1) 
 {
     XL <- data.frame(yy)
     p <- ncol(XL)
     XN <- nrow(XL)
     Xn <- length(levels(factor(XL$id)))
     From<-xy
     To<-xf
     
    if (From<=To && To<=Xn && From>=1){
    if ((From+4)>Xn){To<-min(To,From+4)};
    if ((To-From)>5){To<-From+4;print("This function print max 5 units")
                       };
       print(XM<-XL[((XL$id[]>=From)&(XL$id[]<=To)),])
       for (i in From:To) {
       XMA<-XL[((XL$id[]==i)&(XL$id[]==i)),]
       if (From<To){print(XMA)}
 NT<-sum(XMA$event[] == 1)
      x1 <- matrix(0, nrow = nrow(XMA), ncol = 1)
         x2 <- matrix(0, nrow = nrow(XMA), ncol = 1)
         y1 <- matrix(0, nrow = nrow(XMA), ncol = 1)
         y2 <- matrix(0, nrow = nrow(XMA), ncol = 1)
         x1[1, 1] <- 0
         x2[1, 1] <- 0
         y1[1, 1] <- 0
         y2[1, 1] <- 0
 dev.new()
       par(bg = "white")
       nom <- as.character(paste("Unit = ", i, "\n"))
       plot(0, 0, xlab = "Calendar time", ylab = "Event number",
            xlim=c(0,max(1.01*XMA$Tcal[])),ylim=c(0,NT+1),
            sub=R.version.string)
  title(main = list("Graphical Representation of Recurrent Event Data", 
        cex = 0.8, font = 2.3, col = "dark blue"))
  mtext(nom, cex = 0.7, font = 2, col = "dark red", line = 1.5)
        mtext("Research Group: AVANCE USE R!", cex = 0.7, font = 2, 
        col = "dark blue", line = 1)
        mtext("Software made by: Carlos Martinez", cex = 0.6, font = 2, 
        col = "dark red", line = 0)
       
       abline(h = 0, col = gray(0.9))
       abline(v = 0, col = gray(0.9))
       abline(v = max(XMA$Tcal[]), col = gray(0.9))
 for (j in 1:nrow(XMA)) {
 x1[j, 1] <-XMA$Tinicio[j]
       x2[j, 1] <-XMA$Tcal[j]
       #print(x2[j, 1])
       color<-colevent
       if ( XMA$event[j]==0) color<-colcensor
       segments(x1[j, 1], j-1, x2[j, 1], j-1,lty = ltyx,lwd = lwdx, col = color)
       #abline(h = y2[r, 1], col = gray(0.9))
       abline(v = x2[j, 1], col = gray(0.9))
       text(x2[j, 1], j-1, labels = x2[j, 1], adj = c(0,1), 
       cex = 0.7, pos = "3")
 }}
 }
 else 
 {print("Review parameters")}
 }
