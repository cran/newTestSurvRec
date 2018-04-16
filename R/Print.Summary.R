Print.Summary <-
function (XX,...) 
{
    XL <- XX
    fit1 <- FitSurvRec(Survrecu(id, time, event) ~ as.factor(group), 
             data = XL,"pena-strawderman-hollander")
    fit2 <- FitSurvRec(Survrecu(id, time, event) ~ 1, 
                       data = XL,"pena-strawderman-hollander")
    Plot.Surv.Rec(XL)
    Dif.Surv.Rec(XL, "all")}
