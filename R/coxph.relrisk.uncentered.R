coxph.relrisk.uncentered <- function (coxph.object, newdata){
    center <- coxph.object$means %*% coef(coxph.object)
    if (!missing(newdata)) 
        lp <- predict(coxph.object, newdata, type = "lp")
    else lp <- coxph.object$linear.predictor
    exp(lp + center)
}
