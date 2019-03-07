function (begin, end, survfit1, ...) 
{
    fits <- c.survfit(survfit1, ...)
    mapply(apply.survfit.risk, begin = begin, end = end, MoreArgs = list(models = fits))
}
