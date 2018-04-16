FitSurvRec <-
function (formula, data, type = "pena-strawderman-hollander", ...) 
    {
    method <- charmatch(type, c("pena-strawderman-hollander", 
                                "wang-chang", "MLEfrailty"), nomatch = 0)
    if (method == 0) {
        stop("Estimator must be pena-strawderman-hollander, wang-chang or MLEfrailty")
    }
    call <- match.call()
    if ((mode(call[[2]]) == "call" && call[[2]][[1]] == as.name("Survrecu")) || 
        inherits(formula, "Survrecu")) {
        stop("formula.default(object): invalid formula")
    }
    m <- match.call(expand.dots = FALSE)
    m$type <- m$... <- NULL
    Terms <- terms(formula, "strata")
    ord <- attr(Terms, "order")
    if (length(ord) & any(ord != 1)) 
        stop("Interaction terms are not valid for this function")
    m$formula <- Terms
    m[[1]] <- as.name("model.frame")
    m <- eval(m, sys.parent())
    n <- nrow(m)
    Y <- model.extract(m, "response")
    #print(Y)  
    ll <- attr(Terms, "term.labels")
    if (method == 1) 
        FUN <- PSH.fit
    if (method == 2) 
        FUN <- WC.fit
    if (method == 3) 
        FUN <- PSH.fit
    if (ncol(m) > 1) {
        group <- m[ll][, 1]
        k <- levels(group)
        ans <- NULL
        for (i in 1:length(k)) {
            temp <- Y[group == k[i], ]
            temp1 <- Survrecu(temp[, 1], temp[, 2], temp[, 3])
            ans[[i]] <- FUN(temp1, ...)
        }
        names(ans) <- k
        oldClass(ans) <- "FitSurvRec"
        attr(ans, "strata") <- length(k)
        attr(ans, "group") <- ll
    }
    else {
        temp <- Survrecu(Y[, 1], Y[, 2], Y[, 3])
        ans <- FUN(temp, ...)
    }
     return(ans)}
