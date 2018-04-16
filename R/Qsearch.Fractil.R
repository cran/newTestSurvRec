Qsearch.Fractil <-
function (fr, qr = 0.5) 
{
    tt <- c(0, fr$time)
    ss <- c(1, fr$surv)
    if (ss[length(ss)] > qr) 
        stop(paste("\noverall survival estimate does not fall below ", 
            qr))
    ans <- min(tt[ss <= qr])
    return(ans)
}
