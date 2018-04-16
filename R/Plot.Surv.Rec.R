Plot.Surv.Rec <-
function(XX,...) 
{
    XL <- XX
    x <- factor(XL$group)
    Factores <- x
    x <- c(levels(x))
    Nivelesdefactores <- matrix(x)
    fit1 <-FitSurvRec(Survrecu(id, time, event) ~ as.factor(group), 
            data = XL, type = "pena-strawderman-hollander")
    fit2 <- FitSurvRec(Survrecu(id, time, event) ~ 1, data = XL, 
                       type = "pena-strawderman-hollander")
    print(data.frame(time=fit1[[1]]$time,n.event=fit1[[1]]$n.event, 
                     Surv_G1=fit1[[1]]$survfunc,
                     std.error=fit1[[1]]$std.error))
     print(data.frame(time=fit1[[2]]$time,n.event=fit1[[2]]$n.event, 
                     Surv_G1=fit1[[2]]$survfunc,
                     std.error=fit1[[2]]$std.error))
     print(data.frame(time=fit2$time,n.event=fit2$n.event, 
                     Surv_G1yG2=fit2$survfunc,
                     std.error=fit2$std.error))
    MedianaGPLECombinada <- Qsearch.Fractil(fit2, 0.5)
    MedianaGPLGrupo01 <- Qsearch.Fractil(fit1[[Nivelesdefactores[1, 
        1]]], 0.5)
    MedianaGPLGrupo02 <- Qsearch.Fractil(fit1[[Nivelesdefactores[2, 
        1]]], 0.5)
    Nomb.grupos <- matrix(c("Pooled Group ", "1er Group ", "2do Group "))
    medianas <- matrix(c(MedianaGPLECombinada, MedianaGPLGrupo01, 
        MedianaGPLGrupo02))
    tabla <- data.frame(Group = Nomb.grupos, Median = medianas)
    print(tabla)
    dev.new()
    plot(fit2$time, fit2$survfunc,  
        xlab = "Time", ylab = "Probability of survival", xlim = c(0, 
            0.95 * max(fit2$time)), ylim = c(0, 1.05), type = "s", 
        col = "blue", lwd = 1, sub = R.version.string)

        title(main = list("Survival Curves for groups with Recurrent Event Data", 
        cex = 0.8, font = 2.3, col = "dark blue"))
        mtext("Research Group: AVANCE USE R!", cex = 0.7, font = 2, 
        col = "dark blue", line = 1)
        mtext("Software made by: Dr. Carlos Martinez", cex = 0.6, font = 2, 
        col = "dark red", line = 0)

    lines(fit1[[Nivelesdefactores[1, 1]]]$time, fit1[[Nivelesdefactores[1, 
        1]]]$survfunc, type = "s", lty = 2, col = "red")
    lines(fit1[[Nivelesdefactores[2, 1]]]$time, fit1[[Nivelesdefactores[2, 
        1]]]$survfunc, type = "s", lty = 3, col = "black")
    legend("topright", c("Pooled Group", "First Group", "Second Group"), 
        col = c("blue", "red", "black"), lty = c(1, 2, 3))
}
